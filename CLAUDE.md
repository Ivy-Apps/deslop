# CLAUDE.md

## Commands

All cabal/GHC commands must run inside the Nix dev shell using our `nix run` commands.

### Fast feedback

While iterating on code, use this instead of a full build:

```bash
nix run .#quick-typecheck
```

It answers exactly one question, "does the project typecheck", by keeping a warm
`ghcid` session over all four components (library, executable, tests,
benchmark). It prints either `All good (70 modules)` and exits 0, or the GHC
errors verbatim and exits 1. There is no need to pipe it through `grep`.

Roughly 0.5s when nothing broke and ~2s when something did, against ~8s for
`nix run .#build`. The first call after a break starts the session and takes
~15s; every call after that is fast, so just run it and keep going.

**This is not a quality gate.** The session is interpreted, so it does not run
tests, does not run `hlint`, does not link, and does not compile with `-O2`. Use
it to converge quickly, then finish with the real checks below. **You are not
done until `nix run .#build`, `nix run .#test` and `nix run .#lint` pass.**

Notes:

- Editing `deslop.cabal` restarts the session, so that check costs ~8s.
- Long error output is truncated; the printed path holds the full text.
- The daemon stops itself after 30 minutes idle. `just stop-ghcid` stops it now.

### Building

```bash
nix run .#build
```

### Running Tests

Run all tests:
```bash
nix run .#test
```

Run tests for a specific module (matches against the root `describe` block):
```bash
nix run .#test -- Lexer
nix run .#test -- Parser
```

### Linting

```bash
nix run .#lint
```

### Test fixtures

`test/fixtures/ts-gitignore-project/` contains real `.gitignore` files, which
this repository's own git honours too. Anything they ignore must be force-added
once, or it will silently never be committed:

```bash
git add -f test/fixtures/ts-gitignore-project/<path>
```

## Coding Conventions

- **Custom Prelude:** The project uses `relude` as a custom prelude and `Text` (Data.Text) is available without importing.
- **Extensions:** Assume `OverloadedRecordDot` is enabled.
- **Function composition:** Prefer the `.` composition operator when idiomatic. For example: Prefer `f . g $ a` over `f $ g a`, prefer `traverse (const . Gen.subsequence $ xs) xs` over `traverse (const (Gen.subsequence names)) names`.
- **Existing type classes:** Prefer using existing type classes and the functions that come out-of-the-box with them. Create instances for those typeclasses for our custom types so we can re-use the standard constructions and avoid re-inventing the wheel. Prefer Category Theory and read [Typeclassopedia](https://wiki.haskell.org/index.php?title=Typeclassopedia) if you have to deal with type classes. 
- **Generalized code:** When a concept generalizes well prefer implementing it using parametric polymorphic functions or creating custom type classes. If you thing a new code is general, ask the user and suggest a general implementation.
