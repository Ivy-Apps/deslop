# Contributing to Deslop

PRs are welcome. There is no obligation to review or merge — decisions are subjective.

**AI-generated PRs will not be considered.** Write real code.

---

## Development Setup

Deslop is written in Haskell (GHC 9.10.3) and uses [Nix](https://nixos.org/) for a reproducible dev environment.

**Recommended — [direnv](https://direnv.net/):**

```bash
direnv allow
```

This automatically enters the Nix dev shell whenever you `cd` into the project.

**Alternative — manual:**

```bash
nix develop
```

All `cabal`/GHC commands must run inside the dev shell.

## Commands

```bash
nix run .#build   # build
nix run .#test    # run all tests
nix run .#lint    # lint (hlint)
```

Run tests for a specific module — the argument matches against the root `describe` block:

```bash
nix run .#test -- Lexer
nix run .#test -- Parser
```

Inside `nix develop`, [`just`](https://github.com/casey/just) provides additional workflows (`just --list` for all of them):

| Command | What it does |
|---------|-------------|
| `just check` | Full local check: hlint, tests, build, plus a real run against the sandbox project |
| `just sandbox` | Regenerate `sandbox/` from `test/fixtures/ts-project-1` for manual testing |
| `just update-golden` | Re-record the hspec-golden snapshots in `.golden/` |
| `just update-deps` | Update the Nix flake inputs and re-freeze cabal dependencies |
| `just update-hie` | Regenerate `hie.yaml` |
| `just fix-hls` | Purge caches and rebuild when the language server misbehaves |

## Project Layout

| Path | Contents |
|------|----------|
| `app/Main.hs` | Executable entry point |
| `src/Deslop/` | Rulebook parsing, code graph, rule enforcement, baseline, problem formatting |
| `src/TypeScript/` | TypeScript lexer, parser, CST, module resolver, tsconfig handling |
| `src/Effects/` | `effectful` effects for CLI, file system, and problem reporting |
| `test/` | hspec suites, plus `test/E2E/` golden tests |
| `test/fixtures/` | Sample TypeScript projects and rulebooks used by tests |
| `.golden/` | Recorded golden output for the E2E tests |
| `npm/` | npm wrapper package and per-platform binary packages |
| `docs/` | Internal references (e.g. the Glob+ pattern language) |
| `examples/rules/` | Example rulebooks shipped for users to copy |

## Coding Conventions

- **Custom prelude:** the project uses [`relude`](https://hackage.haskell.org/package/relude), so `Text` is available without importing.
- **Extensions:** `GHC2024` with `OverloadedRecordDot` enabled.
- **Warnings are errors:** the build runs with `-Wall -Werror`.
- Formatting is handled by [fourmolu](https://github.com/fourmolu/fourmolu) (see `fourmolu.yaml`).

## Testing

Golden tests compare CLI output against snapshots in `.golden/`. If you intentionally change output formatting, re-record them with `just update-golden` and review the diff before committing.

## Releasing

Releases are cut by pushing a `v*` tag. The [`release.yml`](.github/workflows/release.yml) workflow builds static Linux binaries (x64, arm64) and a macOS arm64 binary, then publishes the platform packages and the `@ivy-apps/deslop` wrapper to npm. The npm version is derived from the tag.
