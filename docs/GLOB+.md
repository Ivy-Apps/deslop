# Glob+

Glob+ (GlobPlus) is a custom pattern format that extends standard glob syntax with named **variables**. It powers the target and clause matching in the Deslop Architectural Rulebook DSL.

## Overview

Standard globs match file paths. Glob+ goes further: it can **capture** parts of a target path and **reuse** them - in any case style - across that rule's clauses.

```
Target:  @/features/**/{{FileName}}Container
Match:   @/features/home/HomeContainer  →  file-name = "Home"

Clause:  {{TARGET_DIR}}/{{FileName}}View
Expands: @/features/home/HomeView
```

A pattern may capture more than one variable:

```
Target:  @/components/{{provider-name}}/{{service-type}}/{{FileName}}View
Match:   @/components/stripe-connect/payment/CheckoutView
         provider-name = "stripe-connect"
         service-type  = "payment"
         file-name     = "Checkout"

Clause:  @/services/{{provider-name}}/{{service-type}}-{{file-name}}
Expands: @/services/stripe-connect/payment-checkout
```

---

## Paths are segments

A module id is a list of `/`-separated **segments**, and a Glob+ pattern is a
list of segment patterns matched against them one for one. Everything about
Glob+ follows from that: `**` is the only token that changes how many segments
a pattern consumes, and every other token consumes exactly one segment, or part
of one.

```
@/components/stripe/CheckoutView   →   [@] [components] [stripe] [CheckoutView]
```

### Wildcards

| Token | Scope | Meaning |
|---|---|---|
| `**` | a whole segment | zero or many segments |
| `*` | inside a segment | zero or more characters, never a `/` |

`**` must stand alone as a segment. `@/a/**View` does not compile, because a
globstar glued to text would make the number of segments a pattern consumes
depend on the path rather than on the pattern. Write `@/a/*View` to match inside
one segment, or `@/a/**/*View` to cross segments.

`*` is an ordinary part of a segment and may sit beside literals and variables:
`use*ViewModel`, `*.spec`, and a bare `*` - the segment whose only part is a
star, matching exactly one segment of any content.

### `**` means zero, so `a/**` matches `a`

```
@/lib/**        matches  @/lib          (** stands for nothing)
                matches  @/lib/jwt
                matches  @/lib/auth/user

@/**/{{F}}View  matches  @/CheckoutView (** stands for nothing)
```

This is why `forbids: "@/internal/{{FileName}}/**"` catches an import of the
module `@/internal/Foo` itself, and not only what sits beneath it.

---

## Variables

A variable is **a name written in a casing**. The spelling determines both: the
words give the variable its identity, and the way they are cased says which form
you want at that spot.

```
{{ProviderName}}   → variable "provider-name", PascalCase
{{providerName}}   → variable "provider-name", camelCase
{{provider-name}}  → variable "provider-name", kebab-case
{{PROVIDER_NAME}}  → variable "provider-name", CONSTANT_CASE
```

All four are the **same variable**. Capture it in one casing and you can use it
in any of the others. This is why `{{FileName}}` and `{{file-name}}` have always
referred to the same value: they are one variable, named `file-name`.

### Casing is inferred, never declared

A token is compiled to whichever casing it is a valid spelling of:

| Casing | Valid spelling |
|---|---|
| PascalCase | `^[A-Z][a-zA-Z0-9]*$` |
| camelCase | `^[a-z][a-zA-Z0-9]*$` |
| kebab-case | `^[a-z0-9]+(-[a-z0-9]+)*$` |
| CONSTANT_CASE | `^[A-Z0-9]+(_[A-Z0-9]+)*$` |

If exactly one matches, that is the casing. If none match, or more than one, the
rule fails to compile.

| Token | Result |
|---|---|
| `{{ProviderName}}` | `provider-name`, PascalCase |
| `{{provider-name}}` | `provider-name`, kebab-case |
| `{{Provider}}` | `provider`, PascalCase - a lone capitalised word is unambiguous |
| `{{provider}}` | **error** - reads as both camelCase and kebab-case |
| `{{PROVIDER}}` | **error** - reads as both PascalCase and CONSTANT_CASE |
| `{{Provider-Name}}` | **error** - not a recognised casing |
| `{{provider_name}}` | **error** - snake_case is not supported |
| `{{HTTPClient}}` | **error** - consecutive capitals have no word boundary |

Single-word names in all-lowercase or all-uppercase are rejected because the two
readings capture different things: as kebab-case, `{{provider}}` would swallow
hyphens and capture `stripe-connect`; as camelCase it would stop at the first
hyphen. Add a word - `{{provider-name}}` or `{{providerName}}` - and the
ambiguity disappears.

`{{HTTPClient}}` is rejected for the same reason: its word boundaries cannot be
determined, so it could not be recognised as the same variable as
`{{http-client}}`. Write `{{HttpClient}}` or `{{http-client}}` instead.

**A file named `HTTPClient.tsx` is fine**, and `{{ProviderName}}` captures it
happily. Patterns are strict and values are lenient, on purpose: you choose what
your rule says, so an ambiguous variable there is a mistake worth stopping for.
You do not choose what the codebase is called, so an ambiguous value is read as
generously as it can be. See
[ADR 7](adr/0007-glob-plus-values-agree-by-spelling-compatibility.md).

### The `{{TARGET_DIR}}` keyword

| Variable | Meaning |
|---|---|
| `{{TARGET_DIR}}` | The directory portion of the matched target file path |

For a target matched at `@/features/home/HomeContainer`, `{{TARGET_DIR}}` expands to `@/features/home`.

The name `target-dir` is reserved in every casing. `{{targetDir}}`,
`{{target-dir}}` and `{{TargetDir}}` are all errors pointing at the one accepted
spelling.

---

## Where a variable may stand

Two rules govern placement, and both exist so that a pattern means the same
thing in a shallow tree as in a deep one.

### A variable must be anchored

In a **target** pattern, a variable may not have `**` on both sides.

```
@/{{provider-name}}/**/{{FileName}}View     ✅ each variable anchored on one side
@/**/{{provider-name}}/{{FileName}}View     ✅ anchored from the end
@/**/{{provider-name}}/**/{{FileName}}View  ❌ nothing says which directory it is
@/**/{{provider-name}}/**                   ❌ same
```

With `**` on both sides, the *path* decides which directory the variable names
rather than the pattern: at depth 2 it would bind one folder, at depth 4 another,
and neither is the one you meant. Anchor it against a literal, or use `*` to fix
the depth.

Because every variable is anchored, its segment is a function of the pattern and
the path length alone. That is a guarantee, not a convention: **no choice the
matcher makes about `**` can change what anything binds.**

A **clause** pattern is exempt. It substitutes variables rather than capturing
them, so by the time anything is matched they are literal text.

### Two variables need a literal between them

```
@/x/{{FileName}}{{ServiceType}}     ❌ no boundary
@/x/{{FileName}}*{{ServiceType}}    ❌ a * can match nothing, so it is not a boundary either
@/x/{{provider-name}}-{{service-type}}   ✅
@/x/{{provider-name}}/{{service-type}}   ✅
```

Where a separator is one that both variables *could* consume, the leftmost binds
greedily:

```
@/x/{{provider-name}}-{{service-type}}
  matching @/x/stripe-connect-payment gives
    provider-name = "stripe-connect"
    service-type  = "payment"
```

Greedy is only a *preference*, though - see the next section.

---

## Matching Semantics

### Target Matching (`matchTarget`)

1. The path is split into segments once.
2. The matcher walks pattern segments against path segments. A `**` tries zero
   segments first, then one, and so on.
3. Each variable occurrence narrows what that variable can name, and a branch
   that leaves it naming nothing is abandoned immediately.
4. A `MatchEnv` is returned containing:
   - `targetDir`: the directory of the matched path.
   - `variables`: a map from each variable name to its value in every casing.

### Agreement chooses the split, it does not merely check it

A variable may appear several times in a target pattern, in the same casing or
different ones:

```
@/components/{{provider-name}}/{{ProviderName}}View
```

After matching, **some one name must spell every occurrence**, or the target does
not match. This is not the same as the occurrences decoding to the same words:
`HTTPClient` decodes to several readings, and it is enough that `http-client` is
one of them.

```
@/components/stripe-connect/StripeConnectView   →  matches, provider-name bound
@/components/http-client/HTTPClientView         →  matches, provider-name = http-client
@/components/aws-s3/AWSS3View                   →  matches, provider-name = aws-s3
@/components/api-2fa/Api2faView                 →  matches, provider-name = api-2fa
@/components/stripe-connect/PaypalView          →  no match, no name spells both
```

Crucially, that requirement takes part in **choosing** how a segment divides,
rather than judging a division picked without it:

```
@/c/{{provider-name}}/{{provider-name}}-{{service-type}}
  matching @/c/stripe/stripe-connect-payment

  the greedy division would be  provider-name = "stripe-connect"
  but the folder already bound  provider-name = "stripe"
  so the next division wins:    service-type  = "connect-payment"
```

A repeated variable is a **narrower** filter, deliberately: it matches strictly
less than two distinct variables would. To *require* the matching file rather
than just skip the ones that do not, use `exists:`:

```yaml
target: "@/components/{{provider-name}}/{{FileName}}View"
exists:
  - module: "{{TARGET_DIR}}/{{ProviderName}}View"
```

This also lets one variable constrain two parts of a path to the same value:
`@/{{provider-name}}/{{provider-name}}-service` matches `@/stripe/stripe-service`
but not `@/stripe/paypal-service`.

### Clause Matching (`matchClause`)

1. A clause is **hydrated** once against the `MatchEnv`: every variable becomes
   literal text, and `{{TARGET_DIR}}` may expand into several segments.
2. The hydrated clause is then matched against each candidate path.

### Case Enrichment

When a target captures `HomeContainer` via `{{FileName}}`, it is read as the
words `["home", "container"]` and all four forms are derived:

| Casing | Value |
|---|---|
| PascalCase | `HomeContainer` |
| camelCase | `homeContainer` |
| kebab-case | `home-container` |
| CONSTANT_CASE | `HOME_CONTAINER` |

Every variable is enriched independently from its own capture, so a rule with
three variables gets twelve usable forms. The casing a variable was *captured*
in always keeps its literal text, so same-casing use is exact even when the
derived forms are a guess.

### Spellings

kebab-case and CONSTANT_CASE mark word boundaries with a separator, so a name
has exactly one spelling in each. PascalCase and camelCase mark boundaries with
a capital, and any word may instead be written wholly in capitals - which is
what an acronym is. So one name has several Pascal spellings:

```
["db", "connection"]  →  DbConnection   DbCONNECTION   DBConnection   DBCONNECTION
```

Reading a Pascal spelling back is therefore a guess, and Glob+ makes it the
**coarsest** one: each run of capitals is one word.

```
DBConnection  →  db-connection          HTTPClient  →  http-client
UserProfile   →  user-profile           IOStream    →  io-stream
```

Two readings cannot be recovered, and are listed under
[Limitations](../README.md#limitations).

---

## Polarity

Writing a variable out in a casing it was not captured in is a guess. Deslop
guesses in whichever direction costs a **false positive** rather than a **false
negative**: a false positive is visible and can be silenced with `exclude`,
`allows` or the baseline, while a rule that quietly stops enforcing is not
visible at all.

| Field | Polarity | A match means | Spellings accepted |
|---|---|---|---|
| `target:` | **Widen** | the rule applies here | any name that spells every occurrence |
| `exclude:` | n/a | the module is dropped | *(no variables, so nothing to guess)* |
| `forbids:` | **Widen** | a violation | **every** spelling of the name |
| `allows:` | **Narrow** | exempt from `forbids` | the canonical spelling only |
| `uses:` | **Narrow** | the rule is satisfied | the canonical spelling only |
| `exists:` | **Narrow** | the rule is satisfied | the canonical spelling only |

`forbids:` and `target:` widen because failing to recognise a spelling there
means a violation goes unreported. `uses:`, `exists:` and `allows:` narrow
because they are the clauses where a *match silences a report* - widening them
could only ever remove one.

```yaml
target: "@/widgets/{{file-name}}"
forbids:
  - import: "@/internal/{{FileName}}/**"
```

matching `@/widgets/db-connection` forbids imports under `@/internal/DbConnection`
**and** `@/internal/DBConnection`. A `uses:` clause naming the same variable
would require `DbConnection`, because a clause that is too easily satisfied
hides a real violation.

**Same-casing use is never a guess**, so polarity only ever bites where a clause
writes a variable in a casing its target did not capture it in.

---

## Pattern Types

Three kinds of pattern appear in a rulebook, and they differ in what they may
contain.

### TargetPattern

Used in the `target:` field. Matches a file path and **captures variables**.

- Supports `*`, `**` and variables.
- Every variable must be anchored, and two variables need a literal between them.
- Does **not** support `{{TARGET_DIR}}` - there is no directory yet, it is derived from the match.

### ClausePattern

Used in `uses:`, `exists:`, `forbids:` and `allows:`. Matches a file path against a **hydrated** environment.

- Supports `*`, `**`, `{{TARGET_DIR}}` and any variable **bound by its rule's target pattern**.
- Referencing an unbound variable is a compilation error, not a wildcard.
- The anchoring rule does not apply: a clause substitutes rather than captures.

### ExcludePattern

Used in `exclude:`. A plain glob.

- Supports `*` and `**` only. Variables are rejected: an exclude pattern filters
  the target and binds nothing, so a variable there could never resolve.

---

## Rulebook Usage

In a `.yaml` rulebook, Glob+ patterns appear in:

| Field | Pattern Type | Description |
|---|---|---|
| `target:` | TargetPattern | Which files the rule applies to; captures variables |
| `exclude:` | ExcludePattern | Files removed from the effective target |
| `uses.import:` | ClausePattern | Imports that must be present |
| `exists.module:` | ClausePattern | Files that must exist (e.g. test or Storybook) |
| `forbids.import:` | ClausePattern | Imports that must not be present |
| `allows.import:` | ClausePattern | Exceptions carved out of a `forbids` clause |

### Example: one variable

```yaml
- id: page-container-wires-view-and-viewmodel
  target: "@/features/**/{{FileName}}Container"
  uses:
    - import: "{{TARGET_DIR}}/{{FileName}}StateEvent"
    - import: "{{TARGET_DIR}}/use{{FileName}}ViewModel"
    - import: "{{TARGET_DIR}}/{{FileName}}View"
```

For a file `@/features/home/HomeContainer`, `{{FileName}}` matches only the portion of the segment before the literal `Container` suffix:
- `{{FileName}}` = `Home`, `{{fileName}}` = `home`, `{{file-name}}` = `home`, `{{FILE_NAME}}` = `HOME`
- `{{TARGET_DIR}}` = `@/features/home`

The `uses:` patterns expand to:
- `@/features/home/HomeStateEvent`
- `@/features/home/useHomeViewModel`
- `@/features/home/HomeView`

### Example: three variables

```yaml
- id: view-model-calls-its-own-provider-service
  target: "@/components/{{provider-name}}/{{service-type}}/use{{FileName}}ViewModel"
  uses:
    - import: "@/services/{{provider-name}}/{{service-type}}-{{file-name}}"
```

For `@/components/stripe-connect/payout/useTransferViewModel`:
- `provider-name` = `stripe-connect`, `service-type` = `payout`, `file-name` = `Transfer`
- the `uses:` pattern expands to `@/services/stripe-connect/payout-transfer`

Because the expected module name is derived from all three variables, a
ViewModel cannot satisfy the rule by importing another provider's service.

---

## Compilation Errors

A rulebook whose patterns do not compile aborts the run before any file is
checked. **Every** error is reported, not just the first, grouped by file and
then by rule in the order they were written:

```
Could not load 2 rulebooks.

deslop/rules/components.yaml
  rule 'provider-components-are-isolated'
    target: "@/components/{{provider}}/**"
      {{provider}} is ambiguous: a single-word name reads as both camelCase and kebab-case.
        Give the variable a name of at least two words, for example:
          {{providerName}}
          {{provider-name}}
  rule 'provider-view-is-anchored'
    target: "@/**/{{provider-name}}/**/{{FileName}}View"
      {{provider-name}} has ** on both sides, so nothing in the pattern says which
        path segment it names. A deeper tree would bind a different directory
        than a shallow one, and neither would be the one you meant.
        Anchor it: drop one of the **, or replace it with * to fix the depth.

deslop/rules/widgets.yaml
  rule 'provider-service-view'
    uses.import: "{{TARGET_DIR}}/{{provider-nam}}Service"
      unknown variable {{provider-nam}}.
        Variables bound by this rule's target: file-name, provider-name, service-type
        Did you mean {{provider-name}}?
```

A rule whose **target** does not compile stays quiet about its clauses: they are
checked against the variables the target binds, and a target that failed never
got to bind any.

---

## Implementation Notes

- Matching is a native segment walk over `Text`. There is no regex engine.
- Parsing is done with **Megaparsec**, per segment, and handles pattern *shape*
  only. What is inside `{{ }}` is carried through verbatim and interpreted by a
  separate validation pass, so casing diagnostics are Deslop's rather than
  megaparsec's.
- The rule, not the individual pattern, is the compilation unit: the target
  compiles first and its bound variables become the scope its clauses validate
  against.
- Ahead-of-time compilation separates parse/validate/compile from the hot
  matching path. A clause is then hydrated once per matched target and reused
  for every candidate path.
- The globstar search tries widths shortest-first, but the anchoring rule makes
  that choice unobservable - it can be replaced by any complete search without
  changing a single binding.
- Within a segment, divisions are enumerated greedy-left and the first that
  satisfies every variable's constraints wins. Backtracking is global across
  segments, so a later segment can reject an earlier segment's division.
- A single `{` that is not followed by another `{` is treated as a literal
  character, not a variable delimiter.
- Reading a PascalCase or camelCase capture is a guess; comparing two captures is
  not. Agreement asks whether some name spells both, which is exact whenever one
  of them is kebab-case or CONSTANT_CASE. See
  [ADR 7](adr/0007-glob-plus-values-agree-by-spelling-compatibility.md).
- Same-casing use is always exact, because the literal capture is preserved in
  its own slot: a captured `HTTPClient` stays `HTTPClient` in a PascalCase clause.

See [ADR 9](adr/0009-glob-plus-matches-path-segments.md) for why matching is
structural, and [ADR 10](adr/0010-rulebook-compilation-is-a-separate-stage.md)
for how a rulebook is compiled.
