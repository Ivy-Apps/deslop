# 1. The Justfile is the source of truth for quality checks

Date: 2026-08-04

## Status

Accepted

## Context

The Quality CI has to run the same checks a developer runs locally, but as
separate steps, so a lint error fails in seconds instead of after a full build.

Two things made the obvious approaches awkward:

1. **There are already two ways to run a check.** The flake exposes
   `apps.{build,test,lint}` (thin wrappers documented in `CLAUDE.md` and used by
   AI agents), and the `Justfile` exposes `just check`. The apps have no
   equivalent for the sandbox integration run, and `just check` was a single
   opaque recipe with no fail-fast granularity.
2. **The `default` dev shell is expensive.** It carries
   `haskell-language-server`, `implicit-hie` and `hspec-golden`. The `lint` app
   and `just` both lived only in that shell, so the cheapest check in CI would
   have pulled the heaviest closure — potentially a from-source HLS build on a
   cache miss — before hlint could say anything.

We also considered mirroring `haskell-template`'s `ci.yaml` wholesale. That
workflow calls `nix build .` and `nix fmt -- --ci`, and this flake defines
neither `packages.default` nor `formatter`. Adopting it would have meant adding
a treefmt/fourmolu setup and reformatting the entire codebase in one commit —
unrelated to the goal of getting a quality gate in place.

## Decision

The `Justfile` holds the check definitions. `check` is a sequence of `lint`,
`build`, `test` and `integration`; each is a recipe CI invokes as its own step.

CI runs every step through the lean `ci` dev shell, so `just` and `hlint` were
added to that shell's `nativeBuildInputs`. `apps.lint` now points at `ci` too,
which makes `nix run .#lint` cheap for agents as a side effect.

The flake's `build`/`test`/`lint` apps remain as the documented agent-facing
entry points. They are convenience wrappers, not the definition of a check.

Each CI step spells out `nix develop .#ci --accept-flake-config -c just <recipe>`
in full, matching `haskell-template`'s `ci.yaml`.

## Rejected: entering the dev shell once

Repeating `nix develop` per step is not free — measured on the runner, each
entry cost ~2.5s of re-evaluation (inflated by the `callCabal2nix` IFD), about
12s across the five steps. Two ways to avoid it were tried and dropped:

- **`defaults.run.shell`** at the job level. Removes the repetition but still
  runs `nix develop` per step, so it saves nothing.
- **`nix print-dev-env .#ci > "$BASH_ENV"`** in a setup step, letting bash
  source the dev environment for every later step. This works — the dump ends
  with `eval "$shellHook"` and a fresh `mktemp -d` for `TMPDIR`, so it is
  equivalent to `nix develop`, and it cut per-step entry from ~1.45s to ~0.015s
  locally. It was still rejected: it is an unusual pattern that needs a
  footnote to read, `print-dev-env` output has a history of shell-compatibility
  bugs (NixOS/nix#10263, #8309 — its `;&` case fallthrough breaks bash 3.2),
  and 12s is a rounding error next to the ~75s toolchain realisation and the
  compile.

If the per-step cost ever becomes material, the maintained
`nicknovitski/nix-develop` action does the same job through `$GITHUB_ENV` and
`$GITHUB_PATH`, which also reaches steps that `use:` third-party actions.

## Consequences

- `just check` locally and the Quality workflow cannot drift: editing a recipe
  changes both.
- Steps fail in cost order — hlint, then compile, then tests, then the
  end-to-end sandbox run.
- The `ci` shell is no longer strictly minimal. It gains `just` and `hlint`,
  which are also realised for the `build` and `test` apps that do not need them.
  The cost is small next to the GHC package set the shell already carries.
- Two entry points for the same work still exist (`nix run .#test` vs
  `just test`). This ADR fixes which one is authoritative; anyone adding a new
  check adds a recipe, and wires an app only if agents need it.
- Formatting is still unchecked. Introducing `nix fmt` and a first fourmolu
  pass remains open, and is deliberately separate from this decision.
