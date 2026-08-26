# 15. Glob+ `..*` is a core primitive, not sugar

Date: 2026-08-26

## Status

Accepted

## Context

[ADR 12](0012-glob-plus-parent-dir-navigates-the-pattern.md) gave a clause `..`,
one directory back. It also recorded the cost: `{{TARGET_DIR}}` is the directory
of the matched *file*, so under a target containing `**` a fixed `../shared/**`
names a different folder at every depth and holds at only one of them. The
README carries three separate limitations about it.

[#218](https://github.com/Ivy-Apps/deslop/issues/218) asked for `..*`, zero or
many directories back, which is the answer to all three - and proposed
implementing it as sugar, expanding one clause into `MAX_DEPTH` copies with
`0, 1, 2, …` repetitions of `..`.

[ADR 14](0014-the-rules-dsl-desugars-into-a-small-core.md) sets four conditions
a feature must meet to be sugar. `..*` fails three of them.

**It is not structural.** A `GlobDto` is unparsed `Text` at desugaring time, so
the pass would have to split on `/` and count segments - reimplementing
`parseSegments` beside the real parser. GHC desugars a renamed, typechecked AST;
it rewrites source text nowhere.

**It is not bounded.** `MAX_DEPTH = 100` stands in for a fact that does not
exist yet: the depth of `{{TARGET_DIR}}`, which is not known until `hydrate`.
The constant is the tell. This is the `Cast` situation - the expansion needs
something a later phase computes.

**It is not compositional.** Repeated `forbids` and `allows` clauses are a
disjunction, so expanding one into many is sound. Repeated `uses` and `exists`
clauses are a **conjunction** - `Deslop.Rule.Enforcer` runs `traverse_` over
them and each must be satisfied on its own. `uses: {{TARGET_DIR}}/..*/shared/x`
would become *"import all hundred of these"* rather than *"import any one of
them"*, silently, and in the false-positive direction.

## Decision

**`..*` is a Glob+ primitive.** `Step` gains a constructor, and the fold that
already resolves `..` resolves it too:

```haskell
data Step a = ParentDir | ParentDirStar | Step a

resolveSteps :: (a -> [b]) -> [Step a] -> NonEmpty [b]
resolveSteps expand = fmap reverse . foldlM cancel []
  where
    cancel done ParentDir     = pure (drop 1 done)
    cancel done ParentDirStar = done :| [drop taken done | taken <- [1 .. length done]]
    cancel done (Step step)   = pure (reverse (expand step) <> done)
```

It is the same `foldl'` lifted into a nondeterminism monad - the move
`checkParentDirs` already makes with `foldlM` in `Either`. Without a `..*` every
step takes `pure`, so the result is a singleton and nothing that existed changed
meaning. The number of resolutions is bounded by how many segments are actually
to the left by then: the real depth, at hydration time, with no constant.

**The alternation is absorbed into `ResolvedClause`**, which becomes a
`NonEmpty ResolvedPattern` - today's record, renamed. `hydrate` and
`matchResolved` keep their exact signatures and `Deslop.Rule.Enforcer` does not
change by one line. A clause stays one clause, so `uses` keeps its meaning.
Holding the alternation here rather than handing it to callers is what keeps
"the alternatives combine by disjunction" decided in one place, where `all`
cannot quietly be written for `any`.

**Every step behind a `..*` must name one directory.** A single `..` is checked
against the one segment it would cancel; a `..*` may cancel any prefix, so all
of them are checked. This reuses `ParentDirPastWildcard` - no new error kind -
and costs nothing, because the rejected cases are worthless:

```
@/client/**/..*/shared/**    the ..* could climb nothing at all: a silent no-op
@/**/a/b/..*/shared/**       equals @/**/shared/**, which the author should write
```

In the second, the widest resolution always subsumes every narrower one, because
the `**` re-matches exactly the segments the `..*` cancelled. Wherever `..*` does
real work, permitting these would change nothing.

**`exists:` rejects `..*`, and now rejects `*` and `**` at compile time too.**
An `exists` clause looks a module up, so it must name exactly one.
`compileClausePattern` gains a `Determinism` parameter beside `Polarity`:

| clause | polarity | determinism |
|---|---|---|
| `forbids` | Widen | Nondeterministic |
| `allows` | Narrow | Nondeterministic |
| `uses` | Narrow | Nondeterministic |
| `exists` | Narrow | **Deterministic** |

The two are orthogonal - polarity is which direction it is safe to guess wrong,
determinism is how many answers there may be - so Glob+ keeps naming properties
of a pattern rather than the DSL keywords that have them.

This moves a check that ran at *run time*. `Deslop.Rule.Enforcer` used to throw
`InvalidRuleConfig` mid-run when an `exists` pattern held a wildcard, and only
for the files that happened to match, which contradicted ADR 10. It now fails
when the rulebook loads, beside every other error in the file.

**In a `uses:` message, `..*` is printed as written**, the way `**` already is.
There is no single module to name, and `renderClausePattern` turns the token into
a literal step before folding, so exactly one rendering comes out and
`Deslop.Problem` is untouched.

## Considered options

- **Desugar into `MAX_DEPTH` copies, banned in `uses`/`exists`.** The issue's
  proposal, made sound. Rejected: it needs a new error explaining a restriction
  that exists only because of how the sugar is implemented, hydrates a hundred
  patterns per matched target, and has the desugarer parsing glob text the real
  parser parses again.
- **`..*` in `exists:` means "at any ancestor".** Uniform with the other three
  clauses, and genuinely useful - "a registry exists at or above me". Rejected
  because `exists` already forbids `*` and `**` for naming more than one module,
  so forbidding `..*` extends a rule that is there rather than inventing one,
  and the alternative would have made `MissingModule` carry a list that reads
  worse the deeper the file sits.
- **`..*` stops at the first thing it cannot climb**, rather than being
  rejected. More permissive. Rejected on ADR 12's own reasoning: it makes the
  `..*` a silent no-op in the common case, which is the false-negative direction
  ADR 9's polarity rule exists to avoid.
- **`hydrate` returns `NonEmpty ResolvedClause`**, exposing the alternation.
  More honest at the signature. Rejected for five Enforcer edits and for leaving
  every present and future caller to re-decide that alternatives are a
  disjunction, where `all` typechecks exactly as well as `any`.
- **Alternation inside `Seg`.** Rejected on ADR 12's grounds: `Seg` answers "how
  many path segments does this consume", and an alternation of differing widths
  does not answer it.
- **One `ClauseKind` parameter** instead of `Polarity` plus `Determinism`.
  Matches the domain 1:1 and makes the properties impossible to mispair.
  Rejected because it puts four DSL keywords into the language-agnostic core,
  and a new clause kind would then mean editing Glob+.
- **Make an `exists` pattern's wildcards unrepresentable**, with a separate
  compiled type. The ADR-12-purist option, and it would delete the enforcer's
  `throwError` rather than merely making it unreachable. Rejected as out of
  scope: `AnyChars` is a plain constructor and cannot be voided the way
  `VarPart Void` is, so it needs a new segment-part type.

## Consequences

- `Deslop.Rule.Enforcer` is unchanged. So is every existing test, other than the
  one that asserted the old run-time `exists` failure, which now asserts the
  compile-time one.
- `ParentDirInTargetPattern`, `ParentDirInExcludePattern` and
  `ParentDirPastWildcard` each carry the navigation token, so a message about a
  `..*` says `..*`. `NondeterministicPattern` is new.
- `..*` is structural only as the exact token, exactly as ADR 12 decided for
  `..`: `..*shared` and `a..*b` remain plain text. A pattern that used `..*` as
  a literal glob - "files beginning with `..`" - would change meaning, which
  nothing is believed to do.
- Properties P27-P31 join `tier 7`. P27 is differential against repeated `..`,
  which needs no model written for it: the feature's whole claim is that it says
  what some ladder of `..` says. P31 pins that a `Deterministic` pattern always
  yields a module, which is what keeps the enforcer's retained `InvalidRuleConfig`
  branch unreachable.
- `fixtures/ts-globplus-project` gains `views-reach-shared-at-any-depth`.
  `BadgeView`, which exists in that fixture precisely because it violates the
  fixed `../shared/format-money` rule *only* on account of its depth, satisfies
  the `..*` one - which is the difference this ADR is about, pinned end to end.
- The three README limitations about `..` and depth now point at `..*` as the
  way out.
