# Deslop

[![npm](https://img.shields.io/npm/v/%40ivy-apps%2Fdeslop)](https://www.npmjs.com/package/@ivy-apps/deslop)
[![npm downloads](https://img.shields.io/npm/dw/%40ivy-apps%2Fdeslop)](https://www.npmjs.com/package/@ivy-apps/deslop)
[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://www.haskell.org/)
[![TypeScript](https://img.shields.io/badge/TypeScript-3178c6?logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![GitHub Stars](https://img.shields.io/github/stars/Ivy-Apps/deslop?style=social)](https://github.com/Ivy-Apps/deslop)
[![Quality](https://github.com/Ivy-Apps/deslop/actions/workflows/quality.yaml/badge.svg?branch=main)](https://github.com/Ivy-Apps/deslop/actions/workflows/quality.yaml)

**Static import-graph analyzer for TypeScript. You write architecture rules in YAML; Deslop checks them on every run.**

You define your architecture once — what modules may import, what they must import, and what companion files must exist (unit tests, Storybook stories). When a rule breaks, Deslop reports exactly what broke and how to fix it, in plain language that both your team and your AI agents can act on.

No AI and no heuristics: it walks the import graph, so the same code always produces the same result.

> [!NOTE]
> Deslop is not a replacement for ESLint or Biome — it's complementary. What Deslop replaces is the architecture enforcement part: the import-boundary rules and companion-file checks you'd otherwise spread across Dependency Cruiser configs and hand-written ESLint plugins.

Learn more at **[deslop.dev](https://deslop.dev)**.

---

## Key Capabilities

- **Flexible targeting** - `target` TS modules minus optional `exclude` using Glob+ patterns 
- **Forbid imports** — `forbids`, direct or transitive, catching violations through any import chain
- **Carve out exceptions** — `allows` whitelists specific imports against a broad `forbids`
- **Require imports** — `uses` enforces mandatory dependencies at the module level
- **Require companion files** — `exists` asserts that a test, story, or sibling module is there
- **Detect import cycles** — circular dependencies are found automatically across your whole module graph, with the exact loop printed
- **Glob+ patterns** - named variables like `{{provider-name}}`, `{{FileName}}` and `{{TARGET_DIR}}` capture parts of a path and reuse them, in any casing, across a rule
- **Plain-language `fix` messages** — every violation tells your team (and your agents) exactly what to do

---

## Quick Start

```bash
npm install --save-dev @ivy-apps/deslop
```

> Or run without installing: `npx @ivy-apps/deslop check .`

**Recommended `package.json` scripts:**

```json
{
  "scripts": {
    "lint:fix": "your favorite linter",
    "deslop": "deslop check .",
    "deslop:fix": "deslop fix . && npm run lint:fix",
    "deslop:baseline": "deslop baseline ."
  }
}
```

Then write your first rulebook in `deslop/rules/` — see [Writing Rules](#writing-rules), or copy one from [`examples/rules/`](./examples/rules/).

### Commands

| Command | What it does |
|---------|-------------|
| `deslop check <project>` | Report all rule violations |
| `deslop fix <project>` | Auto-fix violations where possible |
| `deslop baseline <project>` | Write `deslop/baseline.yaml` to silence current violations |

### CI with GitHub Actions

<details>
<summary>Example GitHub Actions workflow</summary>

```yaml
name: Architecture Check

on:
  push:
    branches: [main]
  pull_request:

jobs:
  deslop:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v6

      - uses: actions/setup-node@v6
        with:
          node-version: 24
          cache: npm

      # Assumes Deslop is in devDependencies
      # and "deslop": "deslop check ." is in your scripts
      - run: npm ci

      - run: npm run deslop
```
</details>

Prebuilt binaries ship for `darwin-arm64`, `linux-x64`, and `linux-arm64`. Windows is not supported yet.

---

## How It Works

You describe your architecture in declarative YAML rulebooks and drop them in `deslop/rules/`. Multiple files are supported — split rules by concern, team, or layer however you like. On every run, Deslop reads all rulebooks and enforces them across your entire codebase.

Rules are concise and self-documenting. A non-engineer can read a rulebook and understand the intended architecture. No plugins to author, no regex to wrestle with — just YAML that says what's allowed and what isn't.

Deslop works with **module IDs** — the aliased import paths your code already uses, like `@/features/auth/AuthService`, rather than relative file paths (not `src/features/auth/AuthService.ts`).

> [!TIP]
> Make sure your project has a `@/` path alias configured in `tsconfig.json` so Deslop can resolve all modules.

### Built-in Checks

Two checks are always on and need no rulebook:

| Rule | What it catches | Auto-fixed by `deslop fix` |
|------|-----------------|----------------------------|
| `no-relative-imports` | `./util` or `../../lib/util` where an alias like `@/lib/util` exists | Yes |
| `no-import-cycles` | Circular imports — `@/a` → `@/b` → `@/c` → `@/a` | No |

They report through the same pipeline as rulebook violations, so `deslop baseline` silences them like anything else. Their baseline keys use the lint format, `{rule-id}#{relative-file-path}`:

```yaml
- "no-import-cycles#src/a.ts"
- "no-relative-imports#src/lib/util.ts"
```

---

## Example: Feature-Sliced (Vertical-Sliced) Architecture

The rulebook below enforces a demo Feature-Sliced architecture and demonstrates all Deslop clause types: `forbids` (direct and transitive), `allows` (exceptions to forbids), `uses` (requires an import), and `exists` (requires a module to exist in the module graph).

```yaml
id: feature-sliced
name: Feature-Sliced Architecture
description: A demo for Feature-Sliced (Vertical-Sliced) architecture.
rules:
  - id: feature-isolation
    description: Features must not import from other features.
    target: "@/features/**" # all TS modules in features
    forbids:
      - import: "@/features/**" # can't import anything from features
    allows: # forbids exception
      - import: "{{TARGET_DIR}}/**" # from own feature dir is fine
    fix: >-
      Promote shared logic to @/components, @/hooks, @/lib or an appropriate shared folder.

  - id: lib-feature-agnostic
    description: src/lib must not be coupled to specific features.
    target: "@/lib/**"
    forbids:
      - import: "@/features/**"
    fix: Promote the violating code to @/lib or an appropriate shared folder.

  # @/components, @/hooks, @/types should be feature-agnostic

  - id: no-server-in-client
    description: Client components must not import server-only modules, even transitively.
    target: "@/components/**"
    forbids:
      - import: "**/*.server"
        transitive: true
      - import: "@/server/**"
        transitive: true # a helper that imports a server action is still a violation
    fix: Move the logic to a Server Component, a server action, or an API route.

  - id: hooks-has-tests
    description: Each hook must have unit tests.
    target: "@/hooks/use{{FileName}}"
    exists:
      - module: "{{TARGET_DIR}}/use{{FileName}}.spec"
    fix: Add a use{{FileName}}.spec.ts test suite in the same directory as the hook.

  - id: tests-test-the-module-under-test
    description: Each test suite must import the TS module that it's testing.
    target: "**/{{FileName}}.spec"
    uses:
      - import: "{{TARGET_DIR}}/{{FileName}}"
    fix: Import the TypeScript module that the test is named after.

  # components/pages has a Storybook using "exists: module"

  - id: no-tests-in-prod
    description: Production code must never import test utilities, even transitively.
    target: "**/*"
    exclude:
      - "**/*.spec"
      - "**/*.stories"
      - "@test/**"
      - "**/vitest.*"
    forbids:
      - import: "@test/**/*"
        transitive: true
      - import: "**/*.spec"
        transitive: true
    fix: Remove the import. If needed in production, extract to a non-test utility.
```

---

## Writing Rules

### Rulebook Structure

Every `.yaml` file in `deslop/rules/` is a rulebook.

```yaml
id: my-rulebook
name: My Rulebook
description: What this rulebook enforces
rules:
  - id: my-rule
    description: What this rule checks
    target: "@/features/**/*View"
    # ... clauses
    fix: How to fix a violation
    example: Optional code example
```

Required on a rulebook: `id`, `name`, `description`, `rules`.
Required on each rule: `id`, `description`, `target`, `fix`.

---

### Targeting Modules

#### `target`

Glob+ pattern selecting which modules the rule applies to.

```yaml
target: "@/app/**/route"        # all API routes
target: "@/features/**/*View"   # all View modules
```

#### `exclude`

Removes modules from the effective target. Accepts a list of Glob+ patterns.

```yaml
target: "@/features/**/*"
exclude:
  - "**/*.spec"
  - "**/*.stories"
```

> **Effective target = `target` − `exclude`**

---

### Glob Syntax

| Pattern | Matches |
|---------|---------|
| `*` | Any string within a single path segment (no `/`) |
| `**` | Any string across any number of path segments |

```yaml
"@/features/**/data/*"   # any module inside any data/ subfolder
"@/app/**/page"          # any page module anywhere under app/
```

---

### Glob+ — Variables in Patterns

Glob+ extends glob with **casing variables** that capture and transform a name from the matched target module.

#### Available variables

| Variable | Casing | Example (captured: `UserAuth`) |
|----------|--------|-------------------------------|
| `{{FileName}}` | PascalCase | `UserAuth` |
| `{{fileName}}` | camelCase | `userAuth` |
| `{{file-name}}` | kebab-case | `user-auth` |
| `{{FILE_NAME}}` | CONSTANT_CASE | `USER_AUTH` |

When `target` contains a casing variable, all four casings are derived automatically — use any of them freely in clause patterns.

**Example:** target `@/features/**/{{FileName}}Container` matches `@/features/home/HomeContainer`.
- Captured name: `Home`
- In clause patterns: `{{FileName}}` → `Home`, `{{fileName}}` → `home`, `{{file-name}}` → `home`, `{{FILE_NAME}}` → `HOME`

Variables are available in `target` and in all clause patterns. `exclude` is a plain glob - it filters the target and captures nothing, so variables are not allowed there.

#### `{{TARGET_DIR}}`

Available in clause patterns only. Expands to the directory of the matched module.

```
target matched:  @/features/home/HomeContainer
{{TARGET_DIR}} → @/features/home
```

> For the full pattern-matching semantics, see [`docs/GLOB+.md`](./docs/GLOB+.md).

---

### Clauses

#### `forbids`

Prevents the target module from importing something.

```yaml
forbids:
  - import: "@/data/http-client"   # direct import forbidden
  - import: "react"
    transitive: true               # indirect imports forbidden too
```

`transitive: true` checks the entire reachable import graph — if the module is reachable via any chain, it's a violation.

Use Glob+ variables to make patterns relative to the matched target:

```yaml
target: "@/features/**/use{{FileName}}ViewModel"
forbids:
  - import: "{{TARGET_DIR}}/{{FileName}}View"   # viewmodel must not import its View
  - import: "@/**/components/**/*"
```

---

#### `allows`

Whitelists imports that would otherwise be caught by a `forbids` clause. Use `allows` to carve out exceptions from a broad `forbids` rule.

**Example — a feature may only import from one other feature:**

```yaml
- id: no-cross-feature-imports
  description: Features must not depend on other features, except auth.
  target: "@/features/**"
  forbids:
    - import: "@/features/**"   # no cross-feature imports
  allows:
    - import: "@/features/auth/**"   # except: checkout needs the auth session
    - import: "{{TARGET_DIR}}/**"    # from own feature folder is fine
  fix: Remove the cross-feature import. Only @/features/auth is allowed.
```

---

#### `uses`

Requires the target module to import something.

```yaml
uses:
  - import: "{{TARGET_DIR}}/{{FileName}}StateEvent"   # must directly import
  - import: "{{TARGET_DIR}}/{{FileName}}View"
    transitive: true                                  # must be in the import chain
```

`transitive: true` passes if the import appears anywhere in the reachable graph, not just as a direct import.

All `uses` entries are required — a missing import is a violation.

---

#### `exists`

Requires a module to exist at a given path.

```yaml
exists:
  - module: "{{TARGET_DIR}}/{{FileName}}View.stories"
  - module: "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
```

> [!NOTE]
> Wildcards (`*`, `**`) are not allowed in `exists` patterns — each entry must resolve to a single deterministic path.

---


---

### Metadata

#### `fix`

Plain-text instructions telling developers (and AI agents) how to resolve a violation. Deslop prints this message alongside every violation it reports.

```yaml
fix: Promote shared logic to @/components, @/hooks, @/lib, or an appropriate shared folder.
```

Keep `fix` actionable — describe what to move, extract, or remove, not just what went wrong.

#### `example`

Optional TypeScript snippet showing what correct code looks like. Used in violation output to guide the fix.

```yaml
example: |
  import { HomeStateEvent } from "@/features/home/HomeStateEvent";
  export function HomeContainer() { ... }
```

---

## Advanced Rules (multi-variable)

Everything above uses one variable: the file name. A rule can capture as many as
the path has meaningful parts.

### Variables are named

`{{FileName}}` is not a special token. It is a variable **named** `file-name`,
written in PascalCase - which is why `{{file-name}}` refers to the same value.
Any name works the same way:

```
{{ProviderName}}   {{providerName}}   {{provider-name}}   {{PROVIDER_NAME}}
```

All four are one variable. Capture it in one casing, use it in any other.

### Capturing several parts of a path

Suppose components are organised by provider and service type:

```
src/components/stripe-connect/payment/CheckoutView.tsx
src/components/stripe-connect/payout/TransferView.tsx
src/components/paypal/payment/RefundView.tsx
```

One target pattern captures all three parts:

```yaml
target: "@/components/{{provider-name}}/{{service-type}}/{{FileName}}View"
```

For `@/components/stripe-connect/payment/CheckoutView` that binds:

| Variable | kebab-case | PascalCase | camelCase | CONSTANT_CASE |
|---|---|---|---|---|
| `provider-name` | `stripe-connect` | `StripeConnect` | `stripeConnect` | `STRIPE_CONNECT` |
| `service-type` | `payment` | `Payment` | `payment` | `PAYMENT` |
| `file-name` | `checkout` | `Checkout` | `checkout` | `CHECKOUT` |

Each variable is enriched independently, so every clause can pick the casing it
needs:

```yaml
- id: view-model-calls-its-own-provider-service
  description: A ViewModel may only talk to its own provider's service module
  target: "@/components/{{provider-name}}/{{service-type}}/use{{FileName}}ViewModel"
  uses:
    - import: "@/services/{{provider-name}}/{{service-type}}-{{file-name}}"
  fix: Import your own provider's service module.
```

`@/components/stripe-connect/payout/useTransferViewModel` must import
`@/services/stripe-connect/payout-transfer`. Because the expected module name is
derived from all three variables, no other provider's service satisfies it.

Variables work in `allows` too - this isolates providers from each other without
naming any of them:

```yaml
- id: providers-are-isolated
  target: "@/components/{{provider-name}}/**"
  forbids:
    - import: "@/components/**"          # no cross-component imports
  allows:
    - import: "@/components/{{provider-name}}/**"   # except within your own provider
  fix: Promote shared code out of the provider folders.
```

### Naming rules

The casing of a variable is inferred from how you spell it, so the name has to
be unambiguous. Deslop refuses to load a rulebook it cannot read with certainty.

| Token | Result |
|---|---|
| `{{ProviderName}}` `{{provider-name}}` `{{PROVIDER_NAME}}` | ✅ one variable, three casings |
| `{{Provider}}` | ✅ a lone capitalised word is PascalCase only |
| `{{provider}}` | ❌ reads as camelCase **and** kebab-case |
| `{{PROVIDER}}` | ❌ reads as PascalCase **and** CONSTANT_CASE |
| `{{Provider-Name}}` `{{provider_name}}` | ❌ not a recognised casing |
| `{{HTTPClient}}` | ❌ consecutive capitals have no word boundary |

Use two or more words and every case resolves. The fix is always in the message:

```
Could not load Rulebook: rule 'providers-are-isolated', target: "@/components/{{provider}}/**"
  {{provider}} is ambiguous: a single-word name reads as both camelCase and kebab-case.
    Give the variable a name of at least two words, for example:
      {{providerName}}
      {{provider-name}}
```

A clause may only use variables its own rule's `target` captures. A typo is
caught at load time rather than silently widening the rule:

```
rule 'view-wires-view-model', uses.import: "{{TARGET_DIR}}/{{provider-nam}}Service"
  unknown variable {{provider-nam}}.
    Variables bound by this rule's target: file-name, provider-name, service-type
    Did you mean {{provider-name}}?
```

### Repeating a variable

The same variable may appear twice, which constrains both places to the same
value - useful when a directory and a file name share a name in different cases:

```yaml
target: "@/components/{{provider-name}}/{{ProviderName}}View"
```

```
@/components/stripe-connect/StripeConnectView   ✅ both say "stripe connect"
@/components/stripe-connect/PaypalView          ❌ they disagree, rule does not apply
```

> For the full pattern-matching semantics, boundary rules and error reference, see [`docs/GLOB+.md`](./docs/GLOB+.md).

---

## Example Rulebooks

Production-ready rulebooks you can copy into your own `deslop/rules/` live in [`examples/rules/`](./examples/rules/):

| File | Architecture |
|------|-------------|
| [`global.yaml`](./examples/rules/global.yaml) | Universal rules that apply to any TypeScript codebase |
| [`mvi.yaml`](./examples/rules/mvi.yaml) | Model-View-Intent — Containers, Views, ViewModels |
| [`clean-architecture.yaml`](./examples/rules/clean-architecture.yaml) | Clean Architecture — domain/application/infrastructure/presentation layers |
| [`feature-sliced-design.yaml`](./examples/rules/feature-sliced-design.yaml) | Feature Sliced Design — strict layer hierarchy |
| [`nextjs-app-router.yaml`](./examples/rules/nextjs-app-router.yaml) | Next.js App Router — server/client boundary, route handlers, server actions |
| [`quality.yaml`](./examples/rules/quality.yaml) | Quality standards — test coverage and Storybook requirements |

> [!IMPORTANT]
> These are examples, not a preset. Each rulebook is independent — copy the ones that fit your project and adapt them. Rulebooks can conflict with each other by design.

---

## Comparison to Alternatives

| Feature | Deslop | ESLint + plugin | Dependency Cruiser |
|---------|--------|-----------------|--------------------|
| Rule format | Declarative YAML | JS config objects | Regex-heavy JS/JSON |
| Typical rule length | ~5 lines | ~20–40 lines of JS | ~10–20 lines of regex |
| Engine | Haskell | JavaScript | JavaScript |
| Forbid dependencies | `forbids` | Yes | `forbidden` |
| Allow exceptions | `allows` | Yes | `allowed` |
| Require a dependency | `uses` | No | `required` |
| Require companion files | `exists` | No | No |
| Circular dependency detection | Built in, always on | `import/no-cycle` plugin rule | `no-circular` rule |
| Transitive checks | `transitive: true` on any rule | Possible via typescript-eslint, at severe IDE/CI performance cost | `reachable` attribute, complex regex config |
| Transitive *require* | `uses` + `transitive: true` | No | No |
| Named path variables | `{{FileName}}`, `{{TARGET_DIR}}` | No | No |
| Fix instructions in output | Structured `fix` field | No | No |
| Correct-code example in rule | `example` field, shown in output | No | Comment text only, not shown |
| Baseline | `deslop baseline` → readable YAML, one key per violation | Bulk suppressions (v9.24+) | Verbose JSON per violation |
| Exclude from target | `exclude` list | Yes | Yes |
| Auto-fix relative imports | Built into `deslop fix` | Third-party plugin required | No |
| Dependency graph visualization | No | No | Yes |
| Windows support | Not yet | Yes | Yes |
| Monorepo / multiple tsconfigs | Run per package; full support WIP | `parserOptions.project` glob array | Run per package |

---

## Contributing

PRs are welcome. There is no obligation to review or merge — decisions are subjective.

**AI-generated PRs will not be considered** because AI code is cheap to generate and expensive to review, and I'm not paying that cost for someone else - see [CONTRIBUTING.md](./CONTRIBUTING.md) for the full reasoning, along with the development setup and build/test commands.

## License

[MIT](./LICENSE) © Ivy Apps Ltd
