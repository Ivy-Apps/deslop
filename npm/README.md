# Deslop

[![GitHub Stars](https://img.shields.io/github/stars/Ivy-Apps/deslop?style=social)](https://github.com/Ivy-Apps/deslop)
[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](https://www.haskell.org/)
[![Quality](https://github.com/Ivy-Apps/deslop/actions/workflows/quality.yaml/badge.svg?branch=main)](https://github.com/Ivy-Apps/deslop/actions/workflows/quality.yaml)

**Static import-graph analyzer for TypeScript. You write architecture rules in YAML; Deslop checks them on every run.**

You define your architecture once — what modules may import, what they must import, and what companion files must exist (unit tests, Storybook stories). When a rule breaks, Deslop reports exactly what broke and how to fix it, in plain language that both your team and your AI agents can act on.

No AI and no heuristics: it walks the import graph, so the same code always produces the same result.

Deslop is complementary to ESLint and Biome. Where it does replace something is architecture enforcement — the import-boundary rules and companion-file checks you'd otherwise spread across Dependency Cruiser configs and hand-written ESLint plugins.

## Install

```bash
npm install --save-dev @ivy-apps/deslop
```

Or run it without installing:

```bash
npx @ivy-apps/deslop check .
```

## Usage

Write your rulebooks as YAML files in `deslop/rules/`, then run:

| Command | What it does |
|---------|--------------|
| `deslop check <project>` | Report all rule violations |
| `deslop fix <project>` | Auto-fix violations where possible |
| `deslop baseline <project>` | Write `deslop/baseline.yaml` to silence current violations |

Recommended `package.json` scripts:

```json
{
  "scripts": {
    "deslop": "deslop check .",
    "deslop:fix": "deslop fix . && npm run lint:fix",
    "deslop:baseline": "deslop baseline ."
  }
}
```

## A rule looks like this

```yaml
id: feature-sliced
name: Feature-Sliced Architecture
rules:
  - id: feature-isolation
    description: Features must not import from other features.
    target: "@/features/**"
    forbids:
      - import: "@/features/**"
    allows:
      - import: "{{TARGET_DIR}}/**" # own feature is always fine
    fix: >-
      Promote shared logic to @/components, @/hooks, @/lib
      or an appropriate shared folder.
```

Rules can forbid imports (directly or transitively), whitelist exceptions, require imports, and require companion files such as tests or Storybook stories. Patterns support casing variables like `{{FileName}}` and `{{TARGET_DIR}}` so a single rule stays relative and reusable.

## Requirements

- Node.js >= 18
- Prebuilt binaries for `darwin-arm64`, `linux-x64`, and `linux-arm64`. Windows is not supported yet.
- A `@/` path alias configured in `tsconfig.json`, so Deslop can resolve your modules

Deslop is free and open source. No license key, no account required — in CI or anywhere else.

## Documentation

- [deslop.dev](https://deslop.dev) — overview and examples
- [Writing rules](https://github.com/Ivy-Apps/deslop#writing-rules) — the full rule reference
- [Example rulebooks](https://github.com/Ivy-Apps/deslop/tree/main/examples/rules) — Clean Architecture, Feature-Sliced, MVI, Next.js App Router, and more
- [Glob+ reference](https://github.com/Ivy-Apps/deslop/blob/main/docs/GLOB%2B.md) — full pattern-matching semantics
- [Issue tracker](https://github.com/Ivy-Apps/deslop/issues)

## License

[MIT](https://github.com/Ivy-Apps/deslop/blob/main/LICENSE) © Ivy Apps Ltd
