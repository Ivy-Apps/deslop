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
- **Existing type classes:** Prefer using existing type classes and the functions that come out-of-the-box with them. Create instances for those typeclasses for our custom types so we can re-use the standard constructions and avoid re-inventing the wheel. Prefer Category Theory and read [Typeclassopedia](https://wiki.haskell.org/index.php?title=Typeclassopedia) if you have to deal with type classes. 
- **Generalized code:** When a concept generalizes well prefer implementing it using parametric polymorphic functions or creating custom type classes. If you thing a new code is general, ask the user and suggest a general implementation.
