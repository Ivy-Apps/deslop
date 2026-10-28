# 25. Nix installs the released binary

Date: 2026-10-28

## Status

Accepted

## Context

Nix users had no package: a NixOS configuration wanting Deslop had to write its
own derivation around the npm platform tarballs, and repeat the version bump
and hashes there on every release. The flake already builds Deslop from source
for development, through `callCabal2nix` over GHC 9.10.

## Decision

The flake exposes `packages.<system>.deslop` (also `default`) and
`overlays.default`, built by `nix/package/default.nix` from the static binary
the GitHub Release ships, not from source. `nix/package/release.json` pins the
version and one SRI hash per system; only the systems listed there expose the
package (`x86_64-linux`, `aarch64-linux`, `aarch64-darwin`), so `nix flake
show` and `nix flake check` never evaluate a missing binary.

`release.yml` ends by calling `nix-bump.yml`, which hashes the new release's
tarballs and opens a PR rewriting `release.json`. It is a final job rather
than a `release: published` workflow because the release is created with
`GITHUB_TOKEN`, and events that token causes start no workflows.

## Considered options

- **Build from source.** Exact and needs no hashes, but every user compiles the
  whole Haskell closure (or relies on a cache this project does not run), and
  the result would not be the binary the npm and GitHub releases ship.
- **Fetch the npm platform tarballs.** Works, and is what downstream wrappers
  did, but couples the Nix package to a second registry when the GitHub Release
  carries the same binaries next to the flake.

## Consequences

- The package lags a release until its bump PR merges. That PR is opened with
  `GITHUB_TOKEN`, so its CI starts only once someone closes and reopens it.
- `x86_64-darwin` and Windows get no package, as the release builds no binary
  for them.
- A binary package is not reproducible from this repository's source; its
  trust rests on the release workflow, as the npm package's does.
