# 7. Glob+ variable values agree by spelling compatibility

Date: 2026-08-14

## Status

Accepted. Refines [ADR 6](0006-glob-plus-variables-are-named-with-inferred-casing.md),
which decided how a variable is *named*; this one decides how its captured
*values* are compared and written back out.

Refined in turn by [ADR 9](0009-glob-plus-matches-path-segments.md). The
agreement rule here survives unchanged, but agreement now *participates in
choosing* how a path divides rather than validating a division already chosen -
the question this ADR left open. `Forbidding` and `Requiring` are renamed
`Widen` and `Narrow`, and a target pattern gains an explicit polarity it never
had here.

## Context

ADR 6 made a variable a name written in a casing, and let a name occur more than
once in a target pattern:

```yaml
target: "@/components/{{provider-name}}/{{ProviderName}}View"
```

Two captures then have to be checked against each other. The first
implementation canonicalised both with `tokenizeCase` and compared the results.
That is wrong, because a run of capitals carries no word boundary:

```
tokenizeCase "HTTPClient"  == ["h","t","t","p","client"]
tokenizeCase "http-client" == ["http","client"]
```

so `http-client/HTTPClientView` did not match, the rule silently stopped
applying to it, and nothing was reported. For a linter, a rule that quietly
covers less than its author believes is the worst outcome available - worse than
one that reports too much, which an author can see and baseline.

Three more cases failed the same way and were found by property tests rather
than by inspection: a CONSTANT_CASE segment carrying a digit (`HTTP2_CLIENT`,
because `isUpper '2'` is false), a Pascal spelling of single-letter words
(`AB`), and a word opening with a digit (`Api2fa`, whose boundary no capital can
mark, since capitalising `2fa` changes nothing).

The tempting reading of this is that decoding a Pascal spelling is impossible:
`HTTPAPI` is a legitimate spelling of both `["http","api"]` and `["httpapi"]`,
and nothing in the string distinguishes them. That is true, and it is also the
wrong problem. Agreement never needs to know what a spelling decodes to. It
needs to know whether two spellings **could denote the same name**, and that
question is decidable.

## Decision

**Patterns are strict; values are lenient.** The rule author controls the
pattern, so an ambiguous one is a compilation error - `{{provider}}` and
`{{HTTPClient}}` are still rejected. The rule author does not control the
codebase, so a value is read as generously as it can be.

A name renders in kebab-case and CONSTANT_CASE exactly one way. It renders in
PascalCase and camelCase several ways, because each word may be capitalised or
shouted:

```
renderings PascalCase ["db","connection"]
  = {DbConnection, DbCONNECTION, DBConnection, DBCONNECTION}
```

Three consequences follow.

- **Occurrences agree when some name spells all of them.** Each occurrence
  proposes the names its spelling could decode to; a name survives only if every
  occurrence's literal text is one of its spellings. kebab-case and
  CONSTANT_CASE each propose exactly one name and it is exact, so the moment a
  target names a kebab folder - the convention this feature exists for - the
  answer is decided by an exact membership test rather than by a guess.

- **Where the name is still ambiguous, the coarsest reading wins:** the fewest
  words, i.e. each run of capitals read as one word. `DBConnection` becomes
  `db-connection` rather than `d-b-connection`. This is the standard acronym
  heuristic, but derived from the rendering model rather than bolted on beside
  it, so there is no second rule to keep in sync.

- **Clause hydration is polarity-directed.** A `forbids:` pattern that matches
  reports a violation, so a spelling it fails to recognise is a violation gone
  unreported; it therefore accepts every spelling of every name the captures
  could have denoted. A `uses:`, `exists:` or `allows:` pattern that matches
  means the rule is satisfied, so a spelling it recognises too eagerly silences a
  real violation; those accept the canonical spelling only. Polarity is fixed at
  the four compile sites in `ruleFromDto`, not chosen at the call site.

Separately, and for the same reason of never being quietly wrong, a target
pattern's capture groups are now numbered in the pass that builds the regex.
`countGlobSlash` mirrored `mapTokensGlob` by hand so that a count could be used
to skip the groups the `**/` idiom opens; that only works when every `**/`
precedes every variable, and produced a silent rotation when one did not.

## Considered options

- **Canonical equality**, the original `tokenizeCase`-both-sides comparison.
  Rejected: it asks a question that has no answer, and answers it with silence.
- **Tightening the capture regexes** to exactly the image of each casing, so
  that `HTTPClient` is simply not captured by `{{ProviderName}}`. Provably exact
  over a precisely defined name domain, and it kills the digit and single-letter
  cases outright. Rejected because it narrows what a rule matches, which is the
  false-negative direction: the rule would still stop applying to
  `HTTPClientView`, just for a tidier reason.
- **Keeping every decoding alive and expanding clauses to an alternation over
  all of them**, rather than picking the coarsest. The only option that also
  rescues `ABTest` against an `a-b-test` module. Rejected because it conflates
  genuinely different names in `uses:` and `exists:`, where a wider pattern is
  satisfied more easily - it buys one false positive back at the cost of real
  false negatives.
- **Uniform hydration**, widening or narrowing everywhere instead of by
  polarity. Simpler to explain, and wrong in one direction or the other:
  narrowing everywhere lets a forbidden import spelled the acronym way through,
  widening everywhere lets a missing import look satisfied.
- **Reporting a conflict** - regex matched, no name spells every occurrence - as
  a violation rather than a non-match. Rejected: a repeated variable is a
  deliberately *tighter* filter, and `{{provider-name}}/{{ProviderName}}View`
  should match less than `{{provider-name}}/{{FileName}}View`, not report on the
  difference. A rule that requires the matching file to exist is already
  expressible with `exists:`.
- **Reporting a rule that matches no module at all**, and reporting a module
  that matches a target's structure but not its declared casings. Both rejected:
  a rule may legitimately be preventive, guarding a layer not yet written, and
  making a rule fit its codebase is the author's job. Deslop enforces rules; it
  does not lint naming.

## Consequences

- **`{{provider-name}}/{{ProviderName}}View` now applies to acronym folders.**
  `http-client/HTTPClientView`, `db-connection/DBConnectionView`,
  `aws-s3/AWSS3View`, `v2-api/V2APIView`, `a-b/ABView` and
  `api-2fa/Api2faView` all match, where before they were skipped in silence.
- **A variable before a `**/` binds to its own segment.** It previously bound to
  the globstar's text - a value containing a `/`, which no capture regex can
  produce - and a repeated variable across a `**/` never matched at all.
- **Cross-casing expansion changed value.** A captured `DBConnection` now yields
  `db-connection` where it used to yield `d-b-connection`, a module that could
  never exist. Nothing could have depended on the old value, but it is a change.
- **Two readings remain unrecoverable, and are reported rather than hidden.**
  `AWSS3Client` does not say where `aws` ends, so it reads as `awss3 client`;
  `ABTest` reads as `ab test`. Where only a Pascal spelling is captured, a clause
  will name a module that does not exist and the rule will report it. That is a
  false positive, it is baselineable, and it is pinned by
  `test/fixtures/ts-casing-project` so that it cannot change unnoticed. Naming
  the folder in the target too removes it, since a kebab occurrence is exact.
- **ADR 6 undercounted the rulebooks it breaks.** See its Consequences, now
  corrected: the intentional break did hit shipped content
  (`examples/rules/clean-architecture.yaml`), and variables in `exclude:`,
  repeated-variable targets and adjacent variables all changed behaviour too.
- The example rulebooks are now loaded by the test suite, which is how that
  breakage should have been caught.
