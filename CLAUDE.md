# CLAUDE.md

## Commands

All cabal/GHC commands must run inside the Nix dev shell using our `nix run` commands.

### The check ladder

1. While iterating: `nix run .#quick-typecheck`, then
   `nix run .#quick-test [-- <filter>]` once it is green, and
   `nix develop .#ci -c just test-js` (~2s) after an `explorer/` `.js` edit.
   They cover every `.hs` and `.js` edit, so never run a full `just check`
   mid-work.
2. Once, before handing back: `nix develop .#ci -c just check`, in the
   background. It already runs the build, the tests and hlint, so never run
   `nix run .#build`, `.#test` or `.#lint` before it.
3. After an edit that touches no code (docs only), or only a `git commit`, run
   nothing: rerun the full check only after a code edit.
4. Several `cabal` or `just` commands in a row share one shell,
   `nix develop .#ci -c bash -c '...'`, instead of a `nix run .#test`,
   `.#build` or `.#lint` each (they wrap those commands), which pays ~7.5s of
   `nix develop` per call.

### Fast feedback

```bash
nix run .#quick-typecheck
nix run .#quick-test -- Lexer    # or no filter for the whole suite
```

Both keep one warm `ghcid` session per worktree over the library, the
executable and the tests (not the benchmark, which only CI compiles), so an edit
costs a reload instead of a recompile and relink.

`quick-typecheck` answers "does the project typecheck". It prints either
`All good (<N> modules)` and exits 0, or the GHC errors verbatim and exits 1.
There is no need to pipe it through `grep`. Roughly 0.5s when nothing broke and
~2s when something did. The first call after a break starts the session and
takes ~10s; every call after that is fast, so just run it and keep going.

`quick-test` typechecks the same way, then runs the hspec suite inside the
session, filtered like `nix run .#test -- <filter>`. After the typecheck's
`All good (<N> modules)` it prints only failures and the summary line, and
exits 0 when they pass, 1 on a GHC error, a failing test or a filter that
matches nothing. A filtered run takes ~3s after an edit,
against ~25s for `nix run .#test -- <filter>`. With no filter it runs the
compiled suite through `cabal test` instead: ~17s with nothing to rebuild,
~37s after an edit low in the module graph, against 50-56s interpreted.

**Neither is a quality gate.** The session is interpreted, so it does not run
`hlint`, does not link, and does not compile with `-O2`: step 2 of the ladder
is the gate.

Notes:

- Editing `deslop.cabal` restarts the session, so that check costs ~8s.
- A new module is invisible to the session until it is registered in
  `deslop.cabal`. Until then the check refuses to report and names the file.
- Long output is truncated; the printed path holds the full text.
- Exit code 2 means no usable verdict; the reason goes to stderr.
- You should not normally need to stop the daemon: it retires itself after 2
  hours idle. To reclaim the ~1GB now, run `nix develop -c just stop-ghcid`
  from the repo root. It stops every daemon on the machine, including ones other
  worktrees are still using, so do not run it as routine cleanup.

### Building

```bash
nix run .#build
```

### Running Tests

The full local quality suite needs the lean `ci` shell, and on a fresh machine
a Hackage package index first (cabal builds a few test dependencies whose
`cabal.project.freeze` pins are newer than nixpkgs):

```bash
nix develop .#ci -c cabal update   # once per machine
nix develop .#ci -c just check
```

It is exactly what CI runs; CI adds only `cabal update` and the benchmark
build, since agents do not touch the benchmark (`just build-bench`).
It starts with `just format-check` (fourmolu); `just format` fixes what it
reports.

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

### Updating the tsc resolution corpus

```bash
nix run .#update-resolution-corpus
```

Re-records what TypeScript itself resolves each case in
`fixtures/typescript/resolution-corpus.json` to, by running
`tsc --traceResolution` over a temp copy of each case. Only the `tsc` field is written; everything else is
authored by hand.

The committed answers are what `Frontend.TypeScript.ResolutionCorpusSpec` judges our
resolver against, so the default suite needs neither node nor a compiler. As
with `.golden`, **a failing corpus test is not a reason to run this**: a diff
here is a claim about what the compiler does. See
[ADR 18](docs/adr/0018-typescript-resolution-is-judged-against-tsc.md).

### Updating the ghc corpus

```bash
nix run .#update-ghc-corpus
```

Re-records what GHC says each case in `fixtures/haskell/ghc-corpus.json`
depends on, by running `ghc -M` over a temp copy of each case. Only the `ghc` field is
written. `Frontend.Haskell.GhcCorpusSpec` judges the Haskell frontend against
it; the same rule applies as above: a failing corpus test is not a reason to
run this. See
[ADR 22](docs/adr/0022-haskell-dependencies-are-judged-against-ghc.md).

### Updating the nix corpus

```bash
nix run .#update-nix-corpus
```

Re-records what Nix's parser says the path literals and search paths of each
case in `fixtures/nix/nix-corpus.json` are, by running `nix-instantiate --parse`
over a temp copy of each case. Only the `nix` field is written.
`Frontend.Nix.NixCorpusSpec` judges the Nix lexer against it; the same rule
applies as above: a failing corpus test is not a reason to run this. See
[ADR 24](docs/adr/0024-nix-paths-are-judged-against-nix-instantiate.md).

### Test fixtures

`fixtures/` is laid out by language, then kind; what no language owns is under
`fixtures/shared/`. Goldens mirror it under `.golden/typescript/`,
`.golden/haskell/` and `.golden/nix/`.

| Path | Holds |
|------|-------|
| `fixtures/typescript/projects/` | whole TypeScript projects (`ts-*`, plus two real apps) |
| `fixtures/typescript/files/` | single `.ts` files read on their own |
| `fixtures/typescript/config/` | standalone `tsconfig.json` files |
| `fixtures/typescript/resolution-corpus.json` | the tsc oracle corpus (ADR 18) |
| `fixtures/haskell/projects/` | whole Haskell projects (`hs-*`) |
| `fixtures/haskell/ghc-corpus.json` | the ghc oracle corpus (ADR 22) |
| `fixtures/nix/projects/` | whole Nix projects (`nix-*`) |
| `fixtures/nix/nix-corpus.json` | the nix-instantiate oracle corpus (ADR 24) |
| `fixtures/shared/rulebook/` | rulebook files, valid and broken |
| `fixtures/shared/static/` | a tree of names, for filesystem effects |

**No spec spells a fixture path**: every root has one helper in
`Fixtures.OnDisk` (`projectIn TypeScript "ts-cycles-project"`, `rulebooks`, ...),
so moving a directory is a one-line change there. A new root gets a new helper;
a new language gets a `Language` constructor and its directory in `languageDir`.

`fixtures/typescript/projects/ts-gitignore-project/` contains real
`.gitignore` files, which this repository's own git honours too. Anything they
ignore must be force-added once, or it will silently never be committed:

```bash
git add -f fixtures/typescript/projects/ts-gitignore-project/<path>
```

`fixtures/nix/projects/nix-nixos-config/` is a vendored snapshot (see its
`VENDORED.md`): never edit its `.nix` files; only its `deslop/` directory is
ours.

## Architecture

Four layers. **Imports only ever point inward** - see
[ADR 13](docs/adr/0013-the-source-tree-is-layered-by-dependency-direction.md)
and [ADR 20](docs/adr/0020-languages-plug-in-through-a-staged-compiler-frontend.md).

```
Deslop.hs          orchestration - detects the language, then runs the core
  └─ Frontend.hs   the registry: the only module that imports a language
       └─ Frontend/<Lang>/   a language: bytes → CST → Deslop.Module, behind CompilerFrontend
            └─ Deslop/       language-agnostic core: Frontend (the class + driver), Module, CodeGraph, Problem, GlobPlus, Rule
                 └─ Effects/  FileSystem/  Git/  Utils  Renderable    infrastructure
```

Where new code goes:

- **`Deslop/`** - anything true of every language. It must not import a
  language. It holds nothing machine-specific either: paths crossing the seam are
  `ProjectRelativePath`, and `ModuleId` is opaque and never rendered. The
  `CompilerFrontend` class is `Deslop.Frontend`; the driver that owns the
  Command, the Baseline, fix write-back and concurrency is
  `Deslop.Frontend.Pipeline`.
- **`Frontend/<Lang>/`** - anything that knows a language's syntax, its config
  (`tsconfig`) or its file extensions. A language is `Frontend/<Lang>.hs`, an
  instance of `CompilerFrontend`, plus its internals under `Frontend/<Lang>/`,
  and one line in `Frontend.supported`. Its `parse` is total and lossless (an
  island grammar: what it does not recognise is kept raw, undecodable bytes
  are one opaque island). Its `lint` returns Findings and never
  reports or writes; its `lower` sees the whole program and produces
  `Deslop.Module`, the seam both sides meet at. Nothing crossing it is a tree: a
  frontend hands over a flat list of modules and the edges between them, having
  already resolved every specifier and named what it resolved to. Module Names
  are spelled with `/` in every language (ADR 19). ADR 20 lists the rest.
- **`Effects/`** - every effect declaration and its interpreter. `Effects.CLI`
  is the only code that writes to a terminal; `UI` composes the text it prints
  and is pure.
- **`FileSystem/`, `Git/`, `Utils`, `Renderable`** - infrastructure. These name
  nothing from the domain.
- One module per pipeline stage, named for its subject. No `Types.hs`-style
  catch-alls.
- The layering is checked, not trusted: `deslop/rules/layering.yaml` states
  it as Rules (ADR 21), and `just deslop-self` - part of `just check` and of CI
  - runs the Deslop just built over this repository. A violation that is a
  real gap, such as a library module without a spec, goes in
  `deslop/baseline.yaml` with `cabal run deslop -- baseline .`; a layering
  violation is fixed, or the Rule is.
- A built-in Rule lives with what it reads: `Deslop.Rule.Lint.*` reads only the
  `ModuleGraph`, `Frontend.TypeScript.Lint.*` touches TypeScript.

Tests mirror `src/`:

- `src/Deslop/Problem/Baseline.hs` → `test/Deslop/Problem/BaselineSpec.hs`, whose
  root `describe` is `"Deslop.Problem.Baseline"`. That is what makes
  `nix run .#test -- Deslop.GlobPlus` select what its name says. Variants suffix
  the module name (`GlobPlusPropSpec`). `test/E2E/` is exempt - it names a scope.
- A helper stays private in its spec until a **second** spec needs it. Then it
  moves to `Fixtures.<full.module.path>` or `Generators.<full.module.path>`.
  `TestUtils` holds only domain-free plumbing.
- Test a module through its public interface, never its internals: compile
  and match Glob+ rather than build its AST, load a rulebook through the
  loader rather than Dto -> desugar -> compile by hand, ask a `GitIgnore` what
  it ignores rather than inspect its rules. A lexer is the exception: it is a
  stage boundary, and keeps its own unit tests. Never golden a derived `Show`
  (`ppShow`); assert the fields, or golden what Deslop prints.
- `Doubles.Frontend` is a fake line-based language for testing what every
  frontend shares - the driver and the registry - without TypeScript. Its
  `lines.project` can warn (`warn: ...`) or be broken (`broken: ...`).
- Shared helpers, so no spec rebuilds them:
  - `Fixtures.Deslop.Problem` builds Lint Problems and Rule Violations
    (`lintProblemAt`, `violationOf`) with total modifiers (`withFix`,
    `describedAs`, `autoFixable`); a record update on `Problem` is partial,
    so use those.
  - `Fixtures.Deslop.Rule.Book` loads rulebook YAML through the loader
    (`loadRulebookBytes`); `Fixtures.Deslop.Rule.Book.Dto` writes it.
  - `Fixtures.Deslop.GlobPlus` compiles and matches Glob+ patterns.
  - `Fixtures.Git.Ignore` loads a `GitIgnore` from in-memory `.gitignore`s.
  - `Fixtures.FileSystem.Path` is the one in-memory project, at `/project`.
  - `Doubles.FileSystem`: `runReadOnly` for a read-only mock, `mockTree` for a
    pure tree a directory walk can run over, `InMemoryFileSystem` for reads
    and writes, and `runWritesInMemory`, which records every write by path.
- `E2E.ProjectGoldenSpec` runs `doWork` in-process through one runner
  (`runDeslopOn`) and goldens what each fixture project prints for check,
  baseline and fix. `E2E.CommandLineSpec` pins what surrounds a run, still
  in-process: the exit code `Deslop.deslop` returns, the reason it logs as an
  `Error` (stderr), and what `paramsParser` answers for `--help`, `--version`
  and bad arguments. Keep it to a handful. No test spawns the `deslop`
  executable: a `build-tool-depends` on it breaks `quick-typecheck`.
- `fixtures/` (repo root) cannot live under `test/` - it would collide with
  `test/Fixtures/` on macOS.

## Coding Conventions

- **Custom Prelude:** The project uses `relude` as a custom prelude and `Text` (Data.Text) is available without importing.
- **Extensions:** Assume `OverloadedRecordDot` is enabled.
- **Function composition:** Prefer the `.` composition operator when idiomatic. For example: Prefer `f . g $ a` over `f $ g a`, prefer `traverse (const . Gen.subsequence $ xs) xs` over `traverse (const (Gen.subsequence names)) names`.
- **Existing type classes:** Prefer using existing type classes and the functions that come out-of-the-box with them. Create instances for those typeclasses for our custom types so we can re-use the standard constructions and avoid re-inventing the wheel. Prefer Category Theory and read [Typeclassopedia](https://wiki.haskell.org/index.php?title=Typeclassopedia) if you have to deal with type classes. 
- **Generalized code:** When a concept generalizes well prefer implementing it using parametric polymorphic functions or creating custom type classes. If you thing a new code is general, ask the user and suggest a general implementation.
