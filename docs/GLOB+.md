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

### The `{{TARGET_DIR}}` keyword

| Variable | Meaning |
|---|---|
| `{{TARGET_DIR}}` | The directory portion of the matched target file path |

For a target matched at `@/features/home/HomeContainer`, `{{TARGET_DIR}}` expands to `@/features/home`.

The name `target-dir` is reserved in every casing. `{{targetDir}}`,
`{{target-dir}}` and `{{TargetDir}}` are all errors pointing at the one accepted
spelling.

---

## Pattern Types

Three kinds of pattern appear in a rulebook, and they differ in what they may
contain.

### TargetPattern

Used in the `target:` field. Matches a file path and **captures variables**.

- Supports `*`, `**` and variables.
- Does **not** support `{{TARGET_DIR}}` - there is no directory yet, it is derived from the match.

### ClausePattern

Used in `uses:`, `exists:`, `forbids:` and `allows:`. Matches a file path against a **hydrated** environment.

- Supports `*`, `**`, `{{TARGET_DIR}}` and any variable **bound by its rule's target pattern**.
- Referencing an unbound variable is a compilation error, not a wildcard.

### ExcludePattern

Used in `exclude:`. A plain glob.

- Supports `*` and `**` only. Variables are rejected: an exclude pattern filters
  the target and binds nothing, so a variable there could never resolve.

### Glob Wildcards

| Token | Meaning |
|---|---|
| `*` | Any sequence of characters except `/` |
| `**` | Any sequence of characters including `/` |

---

## Matching Semantics

### Target Matching (`matchTarget`)

1. The TargetPattern is compiled into a regex. Each variable occurrence becomes a capture group typed by its casing:
   - PascalCase → `([A-Z][a-zA-Z0-9]*)`
   - camelCase → `([a-z][a-zA-Z0-9]*)`
   - kebab-case → `([a-z0-9-]+)`
   - CONSTANT_CASE → `([A-Z0-9_]+)`
2. The pattern is matched against the full file path.
3. Each capture is grouped under its variable's name, and **all four case variants** are derived for it via tokenization.
4. A `MatchEnv` is returned containing:
   - `targetDir`: the directory of the matched path.
   - `variables`: a map from each variable name to its value in every casing.

### Clause Matching (`matchClause`)

1. The ClausePattern is compiled into a list of chunks: static regex fragments and variable references.
2. At match time, each variable chunk is resolved from the `MatchEnv`:
   - `{{TARGET_DIR}}` → `env.targetDir` (regex-escaped)
   - a variable → its value in the requested casing (regex-escaped)
3. Chunks are concatenated into a full regex and matched against the candidate path.

### Case Enrichment

When a target captures `HomeContainer` via `{{FileName}}`, the tokenizer splits it into `["home", "container"]` and derives all four forms:

| Casing | Value |
|---|---|
| PascalCase | `HomeContainer` |
| camelCase | `homeContainer` |
| kebab-case | `home-container` |
| CONSTANT_CASE | `HOME_CONTAINER` |

Every variable is enriched independently from its own capture, so a rule with
three variables gets twelve usable forms.

### A variable used more than once

A variable may appear several times in a target pattern, in the same casing or
different ones:

```
@/components/{{provider-name}}/{{ProviderName}}View
```

Each occurrence gets its own capture group. After matching, all captures of that
variable must denote the same name, or the target does not match:

```
@/components/stripe-connect/StripeConnectView   →  matches, provider-name bound
@/components/stripe-connect/PaypalView          →  no match, the two disagree
```

This also lets one variable constrain two parts of a path to the same value:
`@/{{provider-name}}/{{provider-name}}-service` matches `@/stripe/stripe-service`
but not `@/stripe/paypal-service`.

### Boundaries between variables

Two variables with nothing between them are rejected, because the regex has no
way to tell where the first one ends:

```
@/x/{{FileName}}{{ServiceType}}   →  error: no boundary between the two variables
```

A separator that both variables *could* consume is allowed, and the leftmost
variable binds greedily:

```
@/x/{{provider-name}}-{{service-type}}
  matching @/x/stripe-connect-payment-service gives
    provider-name = "stripe-connect-payment"
    service-type  = "service"
```

If that is not what you want, separate them with a character neither casing can
contain, such as `/` or `.`.

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
checked. Every message names the rule and the field it came from:

```
Could not load Rulebook: rule 'provider-components-are-isolated', target: "@/components/{{provider}}/**"
  {{provider}} is ambiguous: a single-word name reads as both camelCase and kebab-case.
    Give the variable a name of at least two words, for example:
      {{providerName}}
      {{provider-name}}
```

An unbound clause variable lists what is actually in scope and suggests the
nearest match:

```
rule 'provider-service-view', uses.import: "{{TARGET_DIR}}/{{provider-nam}}Service"
  unknown variable {{provider-nam}}.
    Variables bound by this rule's target: file-name, provider-name, service-type
    Did you mean {{provider-name}}?
```

---

## Implementation Notes

- Parsing is done with **Megaparsec**, which handles pattern *shape* only. What is inside `{{ }}` is carried through verbatim and interpreted by a separate validation pass, so casing diagnostics are Deslop's rather than megaparsec's.
- The rule, not the individual pattern, is the compilation unit: `compileTargetPattern` runs first and its `boundVars` become the scope that `compileClausePattern` validates against.
- Ahead-of-time compilation separates parse/validate/compile from the hot matching path.
- Regex engine: **TDFA** (`Text.Regex.TDFA`) operating directly on `Text`.
- Adjacent static chunks in a CompiledClausePattern are merged at compile time to minimize allocations on the hot path.
- A single `{` that is not followed by another `{` is treated as a literal character, not a variable delimiter.
- Cross-casing conversion is lossy for acronyms and CONSTANT_CASE values: a captured `DBConnection` yields `d-b-connection`. Same-casing use is always exact, because the literal capture is preserved in its own slot.
