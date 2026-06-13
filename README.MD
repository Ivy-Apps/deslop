# Deslop

[![npm](https://img.shields.io/npm/v/%40ivy-apps%2Fdeslop)](https://www.npmjs.com/package/@ivy-apps/deslop)
[![npm downloads](https://img.shields.io/npm/dw/%40ivy-apps%2Fdeslop)](https://www.npmjs.com/package/@ivy-apps/deslop)
[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://www.haskell.org/)
[![TypeScript](https://img.shields.io/badge/TypeScript-3178c6?logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![GitHub Stars](https://img.shields.io/github/stars/Ivy-Apps/deslop?style=social)](https://github.com/Ivy-Apps/deslop)

Deterministic architecture and quality guardrails for TypeScript, defined in YAML.

Deslop is a static import graph analyzer for TypeScript. You define rules in YAML — what modules can import, what they must import, and what companion files must exist (e.g. unit tests, Storybook stories). It enforces them on every run and reports violations with plain-language fix instructions. It replaces what you'd otherwise cobble together with Dependency Cruiser + custom ESLint rules — in a single, readable declarative DSL.

Learn the full Rules YAML DSL at **[deslop-docs](https://github.com/Ivy-Apps/deslop-docs)** or check **[deslop.dev](https://deslop.dev)** for an overview.

> [!NOTE]
> Deslop is not a replacement for ESLint, Biome, or other linters — it's a complementary tool. Use it alongside your existing setup. Where Deslop replaces something is architecture enforcement: the import boundary rules and companion-file checks you'd otherwise spread across Dependency Cruiser configs and custom ESLint rules.

---

## Key Capabilities

- **Forbid imports** `forbids` — direct and transitive, catching violations through any import chain
- **Require imports** `uses` — enforce mandatory dependencies at the module level
- **Require file existence** `exists` — assert that test files, Storybook stories, or any companion module exist alongside a given module
- **Glob+ patterns** — casing variables like `{{FileName}}` and `{{TARGET_DIR}}` make rules relative and reusable
- **Plain-language `fix` messages** — every violation tells your team (and your AI agents) exactly what to do

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
    "deslop": "deslop check .",
    "deslop:fix": "deslop fix . && npm run lint:fix",
    "deslop:baseline": "deslop baseline ."
  }
}
```

### Commands

| Command | What it does |
|---------|-------------|
| `deslop check <project>` | Report all rule violations |
| `deslop fix <project>` | Auto-fix violations where possible |
| `deslop baseline <project>` | Write `deslop/baseline.yaml` to silence current violations |

> [!TIP]
> Use `baseline` for known violations you're not fixing right now. For false positives, narrow the rule's `target` with `exclude` instead.

---

## Example Rule

Drop your rules in `deslop/rules/`. Here's one that applies to every TypeScript project:

```yaml
id: project-quality
name: Project Quality
description: Baseline quality rules for any TypeScript project.
rules:
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
    fix: Remove the import. If the utility is needed in production, extract it to a non-test module.
```

`transitive: true` means Deslop walks the full import graph — if a production helper imports a test utility, that's caught too, not just direct imports.

For the full DSL — `forbids`, `allows`, `uses`, `exists`, Glob+ variables, and more — see **[deslop-docs](https://github.com/Ivy-Apps/deslop-docs)**.

---

## Installation

```bash
npm install --save-dev @ivy-apps/deslop
```

Deslop is free. No license key, no account required.

---

## Development Setup

The project uses [Nix](https://nixos.org/) for a reproducible dev environment.

**Recommended — [direnv](https://direnv.net/):**

```bash
direnv allow
```

This automatically enters the Nix dev shell whenever you `cd` into the project.

**Alternative — manual:**

```bash
nix develop
```

### Commands

```bash
nix run .#build   # build
nix run .#test    # run all tests
nix run .#lint    # lint
```

Run tests for a specific module:

```bash
nix run .#test -- Lexer
nix run .#test -- Parser
```

---

## Contributing

PRs are welcome. There is no obligation to review or merge — decisions are subjective.

**AI-generated PRs will not be considered.** Write real code.
