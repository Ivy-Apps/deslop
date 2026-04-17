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

