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
- A new module is invisible to the session until it is registered in
  `deslop.cabal`. Until then the check refuses to report and names the file.
- Long error output is truncated; the printed path holds the full text.
- Exit codes: 0 typechecks, 1 GHC errors, 2 no usable verdict (the reason goes
  to stderr).
- You should not normally need to stop the daemon: it retires itself after 30
  minutes idle. To reclaim the ~700MB now, run `nix develop -c just stop-ghcid`
  from the repo root. It stops every daemon on the machine, including ones other
  worktrees are still using, so do not run it as routine cleanup.

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

### Updating golden tests

```bash
nix run .#update-golden
```

Re-records every hspec-golden snapshot in `.golden/` from the current CLI
output, and stages the result.

**A failing golden test is not a reason to run this.** It means the output
changed. Work out whether that change was intended first; only then re-record,
and read the resulting `git diff` before committing it. Re-recording to make a
red test green throws away the only thing the snapshot was protecting.

Note that it starts by emptying `.golden/`, so a run that fails to compile
leaves the directory empty. `git checkout .golden` restores it.

### Test fixtures

`fixtures/ts-gitignore-project/` contains real `.gitignore` files, which
this repository's own git honours too. Anything they ignore must be force-added
once, or it will silently never be committed:

```bash
git add -f fixtures/ts-gitignore-project/<path>
```

## Architecture

Four layers. **Imports only ever point inward** - see
[ADR 13](docs/adr/0013-the-source-tree-is-layered-by-dependency-direction.md).

```
Deslop.hs          orchestration - the only module that knows both a language and the core
  └─ Deslop/       language-agnostic core: AST, CodeGraph, Problem, GlobPlus, Rule
       └─ TypeScript/    a language frontend: bytes → Tokens → CST → Deslop.AST
            └─ Effects/  FileSystem/  Git/  Utils  Renderable    infrastructure
```

Where new code goes:

- **`Deslop/`** - anything true of every language. It must not `import
  TypeScript`; `grep -rn "^import TypeScript" src/Deslop/` returning nothing is
  the check.
- **`TypeScript/`** - anything that knows the syntax, `tsconfig`, or file
  extensions. A new language is a new top-level directory ending in a
  `<Lang>.AST` that produces `Deslop.AST`, the seam both sides meet at.
- **`Effects/`** - every effect declaration and its interpreter. `Effects.CLI`
  is the only code that writes to a terminal; `UI` composes the text it prints
  and is pure.
- **`FileSystem/`, `Git/`, `Utils`, `Renderable`** - infrastructure. These name
  nothing from the domain.
- One module per pipeline stage, named for its subject. No `Types.hs`-style
  catch-alls.
- A built-in Rule lives with what it reads: `Deslop.Rule.Lint.*` reads only the
  `ModuleGraph`, `TypeScript.Lint.*` touches TypeScript.

Tests mirror `src/`:

- `src/Deslop/Problem/Baseline.hs` → `test/Deslop/Problem/BaselineSpec.hs`, whose
  root `describe` is `"Deslop.Problem.Baseline"`. That is what makes
  `nix run .#test -- Deslop.GlobPlus` select what its name says. Variants suffix
  the module name (`GlobPlusPropSpec`). `test/E2E/` is exempt - it names a scope.
- A helper stays private in its spec until a **second** spec needs it. Then it
  moves to `Fixtures.<full.module.path>` or `Generators.<full.module.path>`.
  `TestUtils` holds only domain-free plumbing.
- `fixtures/` (repo root) holds the sample TypeScript projects. It cannot live
  under `test/` - it would collide with `test/Fixtures/` on macOS.

## Coding Conventions

- **Custom Prelude:** The project uses `relude` as a custom prelude and `Text` (Data.Text) is available without importing.
- **Extensions:** Assume `OverloadedRecordDot` is enabled.
- **Function composition:** Prefer the `.` composition operator when idiomatic. For example: Prefer `f . g $ a` over `f $ g a`, prefer `traverse (const . Gen.subsequence $ xs) xs` over `traverse (const (Gen.subsequence names)) names`.
- **Existing type classes:** Prefer using existing type classes and the functions that come out-of-the-box with them. Create instances for those typeclasses for our custom types so we can re-use the standard constructions and avoid re-inventing the wheel. Prefer Category Theory and read [Typeclassopedia](https://wiki.haskell.org/index.php?title=Typeclassopedia) if you have to deal with type classes. 
- **Generalized code:** When a concept generalizes well prefer implementing it using parametric polymorphic functions or creating custom type classes. If you thing a new code is general, ask the user and suggest a general implementation.
