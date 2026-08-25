# Development

Everything you need to build, test and lint Deslop locally. For the rules that
govern what gets reviewed and merged, see [CONTRIBUTING.md](../CONTRIBUTING.md).

## Setup

Deslop is written in Haskell (GHC 9.10.3) and uses [Nix](https://nixos.org/) for
a reproducible dev environment.

### 1. Install Nix

**Recommended - the [Determinate Nix installer](https://github.com/DeterminateSystems/nix-installer):**

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

It turns flakes on out of the box, which this project needs, and it can be
undone with `/nix/nix-installer uninstall`. It is also what CI uses, so your
machine matches [`quality.yaml`](../.github/workflows/quality.yaml).

The upstream installer works too, but then you have to enable
`nix-command flakes` in `nix.conf` yourself.

### 2. Enter the dev shell

**Recommended - [direnv](https://direnv.net/):**

```bash
curl -sfL https://direnv.net/install.sh | bash   # binary install
```

Then hook it into your shell (`~/.zshrc` for zsh, `~/.bashrc` for bash) and
allow this project once:

```bash
eval "$(direnv hook zsh)"
direnv allow
```

From then on the Nix dev shell is entered automatically whenever you `cd` into
the project.

**Alternative - manual:**

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

Run tests for a specific module - the argument matches against the root
`describe` block:

```bash
nix run .#test -- Lexer
nix run .#test -- Parser
```

While iterating, `nix run .#quick-typecheck` answers "does this typecheck" in
about half a second by keeping a warm `ghcid` session. It is an inner loop, not
a gate: it does not run tests, lint or link. You are not done until `build`,
`test` and `lint` pass.

Inside `nix develop`, [`just`](https://github.com/casey/just) provides
additional workflows (`just --list` for all of them):

| Command | What it does |
|---------|-------------|
| `just check` | Full local check: hlint, tests, build, plus a real run against the sandbox project |
| `just sandbox` | Regenerate `sandbox/` from `fixtures/ts-project-1` for manual testing |
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
| `fixtures/` | Sample TypeScript projects and rulebooks used by tests |
| `.golden/` | Recorded golden output for the E2E tests |
| `npm/` | npm wrapper package and per-platform binary packages |
| `docs/` | Internal references (e.g. the Glob+ pattern language) |
| `examples/rules/` | Example rulebooks shipped for users to copy |

The source tree is layered and imports only ever point inward. See
[ADR 13](adr/0013-the-source-tree-is-layered-by-dependency-direction.md) for
what that means and where new code goes.

## Coding Conventions

- **Custom prelude:** the project uses [`relude`](https://hackage.haskell.org/package/relude), so `Text` is available without importing.
- **Extensions:** `GHC2024` with `OverloadedRecordDot` enabled.
- **Warnings are errors:** the build runs with `-Wall -Werror`.
- Formatting is handled by [fourmolu](https://github.com/fourmolu/fourmolu) (see `fourmolu.yaml`).

[`CLAUDE.md`](../CLAUDE.md) documents the architecture, module naming and test
layout in more detail. It is written for AI agents, and it is also the fastest
description of how this codebase is built.

## Testing

Tests mirror `src/`: `src/Deslop/Problem/Baseline.hs` is tested by
`test/Deslop/Problem/BaselineSpec.hs`, whose root `describe` is
`"Deslop.Problem.Baseline"`.

Golden tests compare CLI output against snapshots in `.golden/`. If you
intentionally change output formatting, re-record them with `just update-golden`
and review the diff before committing.

## Releasing

Releases are cut by pushing a `v*` tag. The
[`release.yml`](../.github/workflows/release.yml) workflow builds static Linux
binaries (x64, arm64) and a macOS arm64 binary, then publishes the platform
packages and the `@ivy-apps/deslop` wrapper to npm. The npm version is derived
from the tag.
