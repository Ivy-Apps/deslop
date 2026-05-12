# Glob+

Glob+ (GlobPlus) is a custom pattern format that extends standard glob syntax with typed **case variables**. It powers the target and rule matching in the Deslop Architectural Rulebook DSL.

## Overview

Standard globs match file paths. Glob+ goes further: it can **capture** a file-name segment from a target path and **reuse** it — in any case style — across rule constraints.

```
Target:  @/features/**/{{FileName}}Container
Match:   @/features/home/HomeContainer  →  FileName = "Home"

Rule:    {{TARGET_DIR}}/{{FileName}}View
Expands: @/features/home/HomeView
```

---

## Pattern Types

### TargetPattern

Used in the `target:` field of a rule. Matches a file path and **extracts variables**.

- Supports `*`, `**`, and `{{FileName}}` / `{{fileName}}` / `{{file-name}}` / `{{FILE_NAME}}` variables.
- Does **not** support `{{TARGET_DIR}}` (there is no directory yet — it is derived from the match).

### ClausePattern

Used in `uses:`, `exists:`, `forbids:`, etc. Matches a file path against a **hydrated** environment.

- Supports everything TargetPattern does, plus:
  - `{{TARGET_DIR}}` — the directory of the matched target file.
  - All four case variables, which expand to the captured file-name value converted to the requested case.

---

## Syntax Reference

### Glob Wildcards

| Token  | Meaning                              |
|--------|--------------------------------------|
| `*`    | Any sequence of characters except `/` |
| `**`   | Any sequence of characters including `/` |

### Case Variables

| Variable        | Case Style    | Example (input: `homeScreen`) |
|-----------------|---------------|-------------------------------|
| `{{FileName}}`  | PascalCase    | `HomeScreen`                  |
| `{{fileName}}`  | camelCase     | `homeScreen`                  |
| `{{file-name}}` | kebab-case    | `home-screen`                 |
| `{{FILE_NAME}}` | CONSTANT_CASE | `HOME_SCREEN`                 |

All four variables refer to the **same captured name** — just rendered in different cases. If a TargetPattern captures `Home` via `{{FileName}}`, then a ClausePattern can reference `{{file-name}}` and it will expand to `home`.

### Rule-Only Variable

| Variable         | Meaning                                              |
|------------------|------------------------------------------------------|
| `{{TARGET_DIR}}` | The directory portion of the matched target file path |

For a target matched at `@/features/home/HomeContainer`, `{{TARGET_DIR}}` expands to `@/features/home`.

---

## Matching Semantics

### Target Matching (`matchTarget`)

1. The TargetPattern is compiled into a regex. Each case variable becomes a typed capture group:
   - `{{FileName}}` → `([A-Z][a-zA-Z0-9]*)`
   - `{{fileName}}` → `([a-z][a-zA-Z0-9]*)`
   - `{{file-name}}` → `([a-z0-9-]+)`
   - `{{FILE_NAME}}` → `([A-Z0-9_]+)`
2. The pattern is matched against the full file path.
3. On success, captured groups are mapped to their casing keys, and **all four case variants** are derived via tokenization.
4. A `MatchEnv` is returned containing:
   - `targetDir`: the directory of the matched path.
   - `casings`: a map from each `Casing` to the corresponding string value.

### Rule Matching (`matchClause`)

1. The ClausePattern is compiled into a list of chunks: static regex fragments and variable references.
2. At match time, each variable chunk is resolved from the `MatchEnv`:
   - `{{TARGET_DIR}}` → `env.targetDir` (regex-escaped)
   - Case variables → `env.casings[casing]` (regex-escaped), or `.*` if not found
3. Chunks are concatenated into a full regex and matched against the candidate path.

### Case Enrichment

When a target captures `HomeContainer` via `{{FileName}}`, the tokenizer splits it into `["home", "container"]` and derives all four forms:

| Casing        | Value             |
|---------------|-------------------|
| PascalCase    | `HomeContainer`   |
| camelCase     | `homeContainer`   |
| kebab-case    | `home-container`  |
| CONSTANT_CASE | `HOME_CONTAINER`  |

This means a TargetPattern that captures via `{{FileName}}` automatically makes `{{file-name}}` and `{{FILE_NAME}}` available in ClausePatterns.

---

## Rulebook Usage

In a `.yaml` rulebook, Glob+ patterns appear in:

| Field             | Pattern Type   | Description                                              |
|-------------------|----------------|----------------------------------------------------------|
| `target:`         | TargetPattern  | Which files the rule applies to; captures variables      |
| `uses:`           | ClausePattern    | Imports that must be present                             |
| `uses-optional:`  | ClausePattern    | Imports that are allowed but not required                |
| `exists:`         | ClausePattern    | Files that must exist (e.g. test or Storybook)           |
| `forbidden.import:` | ClausePattern  | Imports that must not be present                         |

### Example

```yaml
- id: page-container-wires-view-and-viewmodel
  target: "@/features/**/{{FileName}}Container"
  uses:
    - "{{TARGET_DIR}}/{{FileName}}StateEvent"
    - "{{TARGET_DIR}}/use{{FileName}}ViewModel"
    - "{{TARGET_DIR}}/{{FileName}}View"
```

For a file `@/features/home/HomeContainer`:
- `{{FileName}}` = `HomeContainer`, `{{fileName}}` = `homeContainer`, etc.
- `{{TARGET_DIR}}` = `@/features/home`

The `uses:` patterns expand to:
- `@/features/home/HomeContainerStateEvent`
- `@/features/home/useHomeContainerViewModel`
- `@/features/home/HomeContainerView`

---

## Implementation Notes

- Parsing is done with **Megaparsec**. `parseTargetPattern` and `parseClausePattern` return typed ASTs.
- Ahead-of-time compilation (`compileTargetPattern`, `compileClausePattern`) separates the parse/compile step from the hot matching path.
- Regex engine: **TDFA** (`Text.Regex.TDFA`) operating directly on `Text`.
- Adjacent static chunks in a CompiledClausePattern are merged at compile time to minimize allocations on the hot path.
- A single `{` that is not followed by another `{` is treated as a literal character, not a variable delimiter.
