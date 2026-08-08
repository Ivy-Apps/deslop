# CLAUDE.md

## Commands

All cabal/GHC commands must run inside the Nix dev shell using our `nix run` commands.

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
