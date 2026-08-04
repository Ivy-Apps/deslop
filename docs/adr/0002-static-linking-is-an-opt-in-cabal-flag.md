# 2. Static linking is an opt-in cabal flag

Date: 2026-08-04

## Status

Accepted

## Context

The release CD ships a fully static Linux binary, built inside an Alpine
container so it links against musl. To get that, `deslop.cabal` declared, in the
`executable deslop` stanza:

```cabal
if os(linux)
  ghc-options: -optl-static -optl-pthread
```

Fully static linking only works against musl. Any *glibc* Linux build — the Nix
`ci` shell used by the Quality workflow, or a contributor running `cabal build`
on Ubuntu — fails at link time with hundreds of undefined references to `fopen`,
`mmap`, `dlopen`, `pthread_*` and `__libc_single_threaded`, because glibc cannot
be statically linked for those. Nothing had built this package with Nix on Linux
before, so the breakage stayed latent until the Quality workflow existed.

## Decision

A manual cabal flag, defaulting off, gates the same stanza in the same
component:

```cabal
flag static
  default: False
  manual:  True
```
```cabal
if os(linux) && flag(static)
  ghc-options: -optl-static -optl-pthread
```

The release workflow's Linux build passes `--flags=static`. Its `--ghc-options=`
string is unchanged, and the macOS build is untouched — `os(linux)` was already
false there.

## Considered options

- **Move the flags to `--ghc-options=` on the release build command.** Tried
  first, and wrong. Command-line `--ghc-options` applies to *every* component in
  the build plan, including the library, which GHC also links when producing its
  shared outputs. The original stanza was scoped to `exe:deslop` alone, so this
  silently widened it and put the working release at risk. A conditional inside
  the stanza keeps the original scope by construction.
- **A separate `cabal.project.release` importing `cabal.project` and adding a
  `package deslop` stanza.** Equivalent in effect, still requires changing the
  release workflow, and splits build configuration across two files.
- **Build the release with `pkgsMusl` under Nix.** The principled fix, and it
  would retire the hand-rolled Alpine/ghcup container entirely. Far larger than
  the problem at hand, and it would rebuild a toolchain that currently works.

## Consequences

- Static linking is now opt-in. Anyone producing a distributable Linux binary
  outside the release workflow must remember `--flags=static`; without it they
  get a glibc-dynamic binary that will not run on other distributions.
- `cabal build` works out of the box on any Linux, which it previously did not.
- The flag is `manual: True`, so cabal's solver will never flip it on its own,
  and `callCabal2nix` in the flake picks up the `False` default.
