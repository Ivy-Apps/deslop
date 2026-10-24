# 20. Languages plug in through a staged compiler frontend

Date: 2026-10-22

## Status

Accepted. Partly supersedes
[ADR 13](0013-the-source-tree-is-layered-by-dependency-direction.md): the
layering and the import direction stand, but a language frontend now lives
under `Frontend/<Lang>/` rather than at the top level, and `Deslop.hs` no longer
knows any language by name.

Haskell, the second language, is
[ADR 21](0021-haskell-is-read-as-one-cabal-package.md).

## Context

TypeScript was Deslop's only language, and `Deslop.hs` drove it by hand: load
`tsconfig.json`, list `.ts` and `.tsx` files, parse each, run the relative
specifier lint, write the file back on `fix`, lower it. Everything a second
language would also need - the Command, the Baseline, write-back, the
`modified` log, concurrency - was interleaved with what only TypeScript needs.
TypeScript had also leaked into the core in four places: a hard-coded
```` ```ts ```` fence in the problem formatter, JavaScript build directories in
`Git.Ignore.alwaysIgnoredNames`, `@/shared/**` in the Glob+ compiler's hints,
and "TypeScript" in `--help`. The count of built-in Rules was a literal `3`.

A first sketch was one opaque method, `pipeline :: fe -> Command -> Eff es
[Module]`. We studied how each language Deslop means to support would fill it:

- **Haskell** needs a module index - which file defines `Data.List.NonEmpty` -
  before any import can be resolved, so lowering is two-phase: parse everything,
  index, then lower.
- **Python** is the same: an import names a package that only the whole
  program can resolve.
- **Nix** resolves a literal `import ./x` or `imports = [ ./a.nix ]` per file,
  by probing the filesystem as TypeScript does: a directory means its
  `default.nix`. What it lacks is evaluation: a computed path, a flake input or
  `<nixpkgs>` is known only once the expression is evaluated.
- **Kotlin** makes a package, which spans files, the unit of architecture, so
  one file is not one Module.
- **Rust** discovers files by walking `mod x;` from each crate root, not by
  listing a directory, and an inline `mod` block is a Module inside a file.
- **TypeScript** resolves by probing the filesystem and needs none of this,
  which is how the opaque sketch looked sufficient.

Every study recommended stages over one opaque method: with `pipeline`, each
language would re-implement the Command, the Baseline and write-back, and get
them subtly different.

## Decision

**A language is a `CompilerFrontend`: a type class of small stages.**
`Deslop.Frontend` declares it, with associated types for the language's
`Config`, its parsed `Unit` and what `lower` can warn about, its `LowerError`:

```
detect      ProjectRoot -> Maybe fe            is this project mine?
loadConfig  ProjectRoot -> Config fe           tsconfig.json, a .cabal file
discover    Config fe -> GitIgnore -> [AbsPath]
parse       Config fe -> AbsPath -> ByteString -> Unit fe       total
render      Unit fe -> ByteString              parse's inverse
lint        Config fe -> Unit fe -> [Finding (Unit fe)]
lower       Config fe -> [Unit fe] -> ([Module], [LowerError fe])  whole-program
```

plus what the core needs to talk about the language: `label`, `lookingFor`,
`codeFence`, `builtInRules` and `ecosystemIgnored`. Each method asks for the
fewest effects it can: `lint` may read the filesystem but cannot report, write
or log.

**The driver owns everything that is the same in every language.**
`Deslop.Frontend.Pipeline.runFrontend` loads the config and `.gitignore`,
discovers, and for each file (32 at a time) reads, parses, lints, reports every
Finding's Problem, applies the repairs whose Problems the Baseline has not
accepted, and - only for `fix` - renders and writes back a file whose bytes
changed, logging it as modified. Then it lowers the repaired units, every one
of them, and logs each `LowerError` as a warning. A frontend never sees the
Command.

**`parse` is total.** Every frontend reads source as an island grammar: what it
recognises - an import, a re-export - is a node, and everything else is kept
verbatim as raw text. There is no input it cannot parse, so there is no
`ParseError`, and no file the walk found is missing from what `lower` sees.
That matters because `lower` builds the module index: a file dropped for not
parsing would vanish from it, and an import of that module would resolve to
something else - in Python, `from . import helpers` would become an attribute
of the package and invent a cycle. Bytes the frontend cannot decode at all,
such as a file that is not UTF-8, become one opaque island holding the bytes
themselves: still lossless, still a module, with no edges, and named in a
`LowerError` so the run warns that its imports were not read.

**A lint says, the driver does.** `lint` returns `Finding`s - a `Problem` and a
`Fix u`, either `NoFix` or `Rewrite (Endo u)`. Reporting, baseline-awareness and
write-back are the driver's, so they cannot drift per language, and a lint is
testable without an effect stack beyond what it reads.

**`lower` is whole-program.** It takes every unit at once, so a language that
needs an index builds it there, and a language whose Module spans files or sits
inside one returns as many Modules as it has.

**The registry is a list, and first match wins.** `Frontend.supported` lists
the languages in the order they are tried, TypeScript first;
`Deslop.Frontend.detectFirst` returns the first that recognises the project.
A polyglot project is read as one language. No match is
`DeslopError.NoFrontendDetected`, naming what each language looked for.
Detection runs before the title line is printed, but the error is raised only
after the Rulebooks load, so a broken Rulebook is still reported first.

**Languages live under `Frontend/<Lang>/`.** `Frontend.hs` is the registry and
the only module that imports a language; `Frontend/TypeScript.hs` is the
instance and `Frontend/TypeScript/**` its internals. A common parent makes
"only the registry imports a language" one rule over one directory, and keeps
the top of `src/` for layers rather than for a growing list of languages.

**The core keeps only what is true of every language.** `.git`, `.direnv`,
`.devenv`, `.cache` and the AI agent folders stay in `Git.Ignore`; the
JavaScript ecosystem's directories moved to `Frontend.TypeScript.Ignore`,
reached through `ecosystemIgnored`. The fence is `codeFence`, the Rule count is
`builtInRules` plus the core's `no-import-cycles`, and the hints and `--help`
text are language-neutral.

## Writing a new frontend

- Put everything under `Frontend/<Lang>/` and end in `Frontend/<Lang>.hs` with
  the instance. Add one line to `Frontend.supported`; that is the only file
  outside your directory you should need to touch.
- `detect` must be cheap: it runs for every language, in order, until one
  matches. A marker is not always certain - `settings.gradle.kts` also exists
  in Java-only builds, Python has four possible markers, and a Haskell `*.cabal`
  file needs a directory listing - so look at as little as recognises the
  project. `lookingFor` says what you look for, for someone whose project has
  none of it.
- `parse` must be total and lossless: `render fe (parse fe cfg p b) == b` for
  every `b`, not only for valid source. Lossless is what lets `fix` rewrite
  one statement and leave every other byte alone; total is what keeps every
  module in `lower`'s index. Write it as an island grammar - recognise only
  what Deslop reads, keep the rest as raw text - and make the unit a sum with
  an opaque case for bytes you cannot decode, as `Frontend.TypeScript.TsUnit`
  does. Something you cannot read, such as Haskell behind `CPP`, is an opaque
  island plus a `LowerError`, not a failure. Test it with a property over
  arbitrary bytes, not only over generated source.
  `parse` is given the `Config` because a project's configuration can decide
  how a file is read: a `.cabal` file's `default-extensions`, such as `CPP` or
  `PackageImports`, change what a Haskell file means.
- `lint` is a side feature: Deslop's focus is architecture rules over the
  Module graph, which `lower` feeds. TypeScript uses `lint` to rewrite relative
  imports and exports. `lint` and its `Fix` are deliberately per unit and single
  file; a repair that spans files is out of scope.
- `lint` returns Findings. It never reports, logs or writes, and it never asks
  about the Baseline. A `Rewrite` must change only what its Problem is about,
  and rewrites in one unit must commute.
- `builtInRules` lists every rule id `lint` can report. Give separate concerns
  separate ids: a Lint Problem's id is `{rule}#{file}`, so a shared id makes
  accepting one Problem silence the other.
- `discover` prunes whatever `isIgnored` says the `GitIgnore` ignores, without
  descending into it. The `GitIgnore` already holds `ecosystemIgnored`, so
  `isIgnored` alone covers what is always ignored, what the ecosystem
  generates and what is gitignored.
- `lower` sees the whole program. Build any module index there, not as global
  state, and from every unit, including the opaque ones. Return a `LowerError`
  for whatever only the whole program shows and the user should know: two
  files claiming one module, a module you could not read, `foo.py` next to
  `foo/__init__.py`. It is a warning; it never fails the run.
- Name every module the same way, inside the project and out. Mint Module
  Names slash-segmented whatever the language writes
  ([ADR 19](0019-module-names-are-slash-segmented-in-every-language.md)), for
  `Resolved` targets and for `External` ones alike: `import Data.Text` reaches
  `External Data/Text`, the same spelling as a project module `Tiny/A`, so a
  Rule for your language is written in one spelling. What the source wrote
  stays on the edge's `specifier`; it is never a name.
- Give every module a canonical name - the head of `names` - that no other
  module in the project shares. A Rule Violation's Problem id is
  `{rulebook}#{rule}#{canonical name}`, so two modules sharing one share every
  Baseline entry: accepting a violation in one silently accepts the same
  violation in the other, and in any module that takes the name later. Qualify
  the canonical name with whatever tells your modules apart, and keep the
  language's own name as an alias so that patterns written against it still
  match. Aliases need not be unique; names of `External` modules never build a
  Problem id, so they need not be either.
  - Haskell: every module is canonically qualified by its component -
    `lib/deslop/Deslop/Module`, and every executable's `Main` is
    `exe/<component>/Main` - aliased by its bare name
    ([ADR 21](0021-haskell-is-read-as-one-cabal-package.md)).
  - Nix: `lib.nix` and `lib/default.nix` are both reached as `./lib`-shaped
    paths, so `lib/default.nix` is canonically `lib/default`, aliased `lib`.
  - Python: `foo.py` beside `foo/__init__.py` is two files and one importable
    name; name the package `foo/__init__`, aliased `foo`, and return a
    `LowerError` for the shadowing.
- `ModuleId` is opaque: mint it from whatever identifies a module in your
  language, and never expect the core to render it. It must agree with the
  names: two modules with the same `ModuleId` are one module, so they carry the
  same names, and one module is never two `ModuleId`s.
- Supply `codeFence` (the Markdown info string, e.g. `haskell`) and `label`.
- Test the instance against the shared driver, the way `Doubles.Frontend` does
  for the fake Lines language, so every frontend gets the same guarantees.

Out of scope for this ADR, and expected to need more: Rust's discovery is a
walk of `mod` declarations from crate roots rather than a file list, and
Kotlin's Module is a package spanning files. Both fit `lower`'s whole-program
shape but may want `discover` to see more than a `GitIgnore`. So are Lint
repairs that span files, and Nix's evaluation-only edges, below.

### Edges only evaluation can name

Nix has dependencies no reading of the source can name:
`import (./. + "/${n}.nix")`, `imports = map (n: ./modules + "/${n}") names`,
`callPackage` on a computed path. The edge exists, but where it points is known
only once the expression is evaluated.

`EdgeTarget` cannot say that today. Its only cases are `Resolved` and
`External`, so such an edge would have to pose as `External`, named by its
expression text. No `forbids` pattern matches that text, so a Rule such as
`forbids: lib/**` would pass a module that may well import `lib/secret.nix` -
and the run would exit 0. A green check nobody earned is the worst outcome a
linter has, which is why this is decided before any Nix frontend exists:

- `EdgeTarget` gains a third case, `Unresolved Specifier`: an edge the
  frontend knows exists and cannot name. It is never an `External`, which
  means a package outside the project.
- A `forbids` Rule whose target has an `Unresolved` edge reports a Problem -
  "cannot be judged" - quoting the statement. It fails the run like any Rule
  Violation and can be baselined, so a team accepts its known dynamic imports
  once, explicitly, rather than having them pass unseen.
- A literal path to a file that does not exist (`import ./gone.nix`) is
  `Unresolved` too, not `External`.
- A frontend may do better than `Unresolved` where it can: a computed path
  with a literal base, `./modules + "/${n}.nix"`, may over-approximate to one
  `Resolved` edge per file the pattern can reach. That is the frontend's
  choice, at the price of cycles that may not be real.

Open, and to be settled with a real Nix fixture when the first frontend that
mints `Unresolved` lands it:

- Whether a transitive `forbids` reports an `Unresolved` edge in a module the
  target only reaches, not just one in the target itself.
- Whether a `uses` Rule counts an `Unresolved` edge as satisfying it.
- What the graph calls an `Unresolved` vertex in a report, without turning its
  expression text into a `ModuleName`.

## Considered options

- **One opaque `pipeline` method.** Smallest class, and the user's first sketch.
  Rejected: every language would own the Command, Baseline and write-back, and
  the five language studies all needed the stages anyway.
- **Per-file `lower`.** Rejected: Haskell, Python and Kotlin cannot name a
  dependency without seeing the whole program.
- **Lints that report through `ReportProblem` and read the Baseline**, as
  `noRelativeSpecifiers` did. Rejected: it made each lint responsible for
  baseline-aware fixing, which is the driver's policy.
- **Running every detected language in a polyglot project.** Deferred: the
  Module graph would need edges across languages first.
- **Languages at the top of `src/`** (`TypeScript/`, `Haskell/`, ...). Rejected:
  the registry rule would have to name every language directory.

## Consequences

- `Deslop/**` imports nothing under `Frontend/**`. Since ADR 21 that is the
  Rule `core-is-language-agnostic` in `deslop/rules/layering.yaml`, which
  `just deslop-self` checks.
- `doWork` keeps its signature; `Command` moved to `Deslop.Command` and is
  re-exported from `Params`.
- Output changed in four places, all intended: a `TypeScript project detected.`
  line after the title; `NoFrontendDetected` instead of a missing-tsconfig
  error; neutral `--help` text; neutral Glob+ hints.
- Lowering now runs after every file is linted rather than per file. Same
  Modules, in the same order.
- A TypeScript file that is not UTF-8 no longer crashes the run: it is lowered
  as a module with no edges, and a warning names it.
- `loadGitIgnore` takes the frontend's generated-directory predicate, so it
  still never walks `node_modules`, and keeps it in the `GitIgnore`, so
  `isIgnored` is the one predicate every walk prunes by.
