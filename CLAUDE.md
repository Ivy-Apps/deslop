# CLAUDE.md

## Commands

All cabal/GHC commands must run inside the Nix dev shell:

```bash
nix develop -c cabal build
nix develop -c cabal test all
nix develop -c hlint .
```

Run a single test file:
```bash
nix develop -c cabal test --test-options "--match /TypeScript.Lexer/"
```

## Coding Conventions

- **Custom Prelude:** The project uses `relude` as a custom prelude and `Text` (Data.Text) is available without importing.
- **Extensions:** Assume `OverloadedRecordDot` is enabled.
