# 6. Glob+ variables are named, with casing inferred from spelling

Date: 2026-08-13

## Status

Accepted

## Context

Glob+ supported exactly one variable per rule. `{{FileName}}`, `{{fileName}}`,
`{{file-name}}` and `{{FILE_NAME}}` were four *casing literals* naming the same
anonymous capture, hard-coded in the parser:

```haskell
pCasing = choice
    [ PascalCase  <$ string "FileName"
    , CamelCase   <$ string "fileName"
    , ConstantCase <$ string "FILE_NAME"
    , KebabCase   <$ string "file-name"
    ]
```

A rule could therefore capture a filename, but nothing else. Real layouts carry
more than one meaningful part in a path - a provider, a service type and a
component name - and expressing a rule over them was impossible:

```yaml
target: "@/components/{{provider-name}}/{{service-type}}/{{FileName}}View"
uses:
  - import: "@/services/{{provider-name}}/{{service-type}}-{{file-name}}"
```

The awkward part is that a variable's token has to carry two things at once: an
identity, so `{{provider-name}}` in a clause resolves to what
`{{ProviderName}}` captured in the target, and a casing, so the same variable
can be matched as `stripe-connect` in a directory and `StripeConnect` in a
filename. Adding a second field (`{{provider-name:kebab}}`) keeps them separate
but makes every pattern noisier, and would leave the four legacy spellings as
permanent special cases.

Reusing the existing `tokenizeCase` shows the two can be one. All four legacy
spellings already collapse to the same word list, so treating the token as a
*name written in a casing* reproduces the old behaviour exactly, with no
compatibility shim:

| Spelling | `tokenizeCase` | canonical name |
|---|---|---|
| `{{FileName}}` | `["file","name"]` | `file-name` |
| `{{fileName}}` | `["file","name"]` | `file-name` |
| `{{file-name}}` | `["file","name"]` | `file-name` |
| `{{FILE_NAME}}` | `["file","name"]` | `file-name` |

## Decision

A Glob+ variable is a **name written in a casing**, and the spelling alone
determines both. Casing is inferred by counting how many of the four casings a
token is a valid spelling of:

```
Pascal   ^[A-Z][a-zA-Z0-9]*$        camel    ^[a-z][a-zA-Z0-9]*$
kebab    ^[a-z0-9]+(-[a-z0-9]+)*$   constant ^[A-Z0-9]+(_[A-Z0-9]+)*$

exactly 1 → that casing    0 → UnrecognisedCasing    2+ → AmbiguousCasing
```

Identity is the token's `tokenizeCase` words joined with `-`. Four decisions
follow from making this total:

- **Ambiguous single-word names are rejected.** `{{provider}}` reads as both
  camelCase and kebab-case, `{{PROVIDER}}` as both PascalCase and CONSTANT_CASE.
  Both are compilation errors suggesting a two-word name. `{{Provider}}` is
  fine - a lone capitalised word is PascalCase only.
- **Consecutive capitals are rejected** in Pascal and camel spellings.
  `{{HTTPClient}}` has no determinable word boundaries, which would make it a
  different variable from `{{http-client}}`. Constant case is all capitals by
  definition, so the check does not apply there.
- **`{{TARGET_DIR}}` is a keyword**, reserved under its canonical name
  `target-dir` in every casing. Only that exact spelling is accepted; in a
  target pattern any spelling of it is an error, since it is derived from the
  matched path rather than captured from it.
- **A clause variable must be bound by its rule's target pattern.** The rule,
  not the individual pattern, is the compilation unit: the target compiles
  first and its variables become the scope its clauses compile in.

Two further points about matching:

- A variable may occur more than once in a target pattern. Each occurrence gets
  its own capture group; after matching, all captures of one variable must
  canonicalise to the same words or the target does not match. This is what
  makes `@/components/{{provider-name}}/{{ProviderName}}View` work, since TDFA
  has no backreferences.
- Two variables with **nothing** between them are rejected - there is no
  boundary for the regex to split on. A literal separator that both variables
  could consume is allowed, and the leftmost variable binds greedily under POSIX
  longest-match. That is documented and pinned by a test rather than forbidden,
  so `{{provider-name}}-{{service-type}}` stays expressible.

Three pattern kinds now have three types. `ExcludePattern = Pattern Void` makes
the `Var` constructor uninhabited, so a variable in an exclude pattern is
unrepresentable rather than merely rejected - excludes filter the target and
bind nothing, so a variable there could never resolve.

## Considered options

- **Explicit casing annotation**, `{{provider:kebab}}`. Unambiguous for every
  name including single words, and orthogonal to identity. Rejected because it
  makes the common case noisier for the sake of the rare one, and would keep the
  four legacy spellings as special cases forever. The single-word gap it would
  close is closed instead by a clear error telling the author to add a word.
- **Defaulting single-word names** to camel/Pascal, or to kebab/constant. Keeps
  `{{provider}}` writable. Rejected because whichever default is chosen is wrong
  half the time and fails silently: as kebab, `{{provider}}` swallows hyphens
  and captures `stripe-connect`; as camel, it stops at the first hyphen. A
  linter that quietly captures the wrong substring is worse than one that
  refuses to start.
- **Keeping the `.*` fallback for unbound clause variables.** No API change -
  clause patterns stay independently compilable, with no need to thread the
  target's variables through `Rulebook`. Rejected because a typo like
  `{{provider-nam}}` would silently turn a precise rule into one matching every
  module, which is the worst failure mode available to a linter. Rejecting it at
  load time also makes every casing lookup total, which is why `MatchEnv` can
  hold `Map VarName CasedName` with a four-field record instead of a partial
  `Map Casing Text`.
- **Rejecting repeated variables** in a target pattern. Simplest to implement
  and fully unambiguous. Rejected because it bans
  `@/components/{{provider-name}}/{{ProviderName}}View`, the kebab-directory /
  Pascal-file convention this feature exists to express.
- **Fixing `tokenizeCase` to treat capital runs as one word**, so `HTTPClient`
  tokenises to `["http","client"]`. Would have made acronym variables work
  instead of being rejected. Rejected as scope creep into shared value
  conversion: rejecting the name at compile time solves the identity problem
  without touching a function that also converts every captured value.

## Consequences

- Existing rulebooks are unaffected. The five project E2E goldens
  (`check-*`, `baseline-*`) are byte-identical across this change, which is the
  mechanical proof; only `rulebook-from-file--page-architecture` moved, and only
  because `Rule`'s `Show` output gained the variable name.
- One intentional break: a rule whose target captures nothing but whose clause
  uses `{{FileName}}` used to compile to `.*` and now fails to load. No rulebook
  in the repository or the fixtures did this.
- A bad pattern aborts the whole run with a message naming the rule, the field
  and the offending token, rather than producing wrong results silently. The
  `UnboundVariable` message lists the variables actually in scope and suggests
  the nearest by edit distance.
- `enrichCasings` is gone. It seeded all four casings from
  `listToMaybe (Map.elems baseMap)` - an arbitrary map element - which was
  harmless only because there was exactly one variable. Each variable is now
  enriched from its own capture.
- `matchClause` fails **closed** when a variable is missing from the
  environment. Compilation makes that unreachable, but an impossible state must
  not be able to widen a rule.
- Cross-casing conversion of *values* remains lossy for acronyms and for
  CONSTANT_CASE, unchanged by this decision. A captured `DBConnection` still
  yields `d-b-connection`.
