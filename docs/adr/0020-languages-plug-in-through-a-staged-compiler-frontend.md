# 20. Languages plug in through a staged compiler frontend

Date: 2026-10-22

## Status

Accepted. Partly supersedes
[ADR 13](0013-the-source-tree-is-layered-by-dependency-direction.md): the
layering and the import direction stand, but a language frontend now lives
under `Frontend/<Lang>/` rather than at the top level, and `Deslop.hs` no longer
knows any language by name.

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
- **Python** and **Nix** are the same: an import names a package or a path that
  only the whole program can resolve.
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
`Config`, its parsed `Unit` and its `ParseError`:

```
detect      ProjectRoot -> Maybe fe            is this project mine?
loadConfig  ProjectRoot -> Config fe           tsconfig.json, a .cabal file
discover    Config fe -> GitIgnore -> [AbsPath]
parse       AbsPath -> ByteString -> Either (ParseError fe) (Unit fe)
render      Unit fe -> ByteString              parse's inverse
lint        Config fe -> Unit fe -> [Finding (Unit fe)]
lower       Config fe -> [Unit fe] -> [Module] whole-program
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
changed, logging it as modified. Parse errors are logged and the file skipped.
Then it lowers the repaired units. A frontend never sees the Command.

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
- `detect` must be cheap and certain: a marker file at the root.
  `lookingFor` says what that marker is, for someone whose project has none.
- `parse` must be lossless: `render fe <$> parse fe p b == Right b`. That is
  what lets `fix` rewrite one statement and leave every other byte alone.
- `lint` returns Findings. It never reports, logs or writes, and it never asks
  about the Baseline. A `Rewrite` must change only what its Problem is about,
  and rewrites in one unit must commute.
- `builtInRules` lists every rule id `lint` can report. Give separate concerns
  separate ids: a Lint Problem's id is `{rule}#{file}`, so a shared id makes
  accepting one Problem silence the other.
- `discover` prunes both what the `GitIgnore` ignores and what
  `ecosystemIgnored` names, without descending into either.
- `lower` sees the whole program. Build any module index there, not as global
  state, and mint Module Names slash-segmented whatever the language writes
  ([ADR 19](0019-module-names-are-slash-segmented-in-every-language.md)).
  `ModuleId` is opaque: mint it from whatever identifies a module in your
  language, and never expect the core to render it.
- Supply `codeFence` (the Markdown info string, e.g. `haskell`) and `label`.
- Test the instance against the shared driver, the way `Doubles.Frontend` does
  for the fake Lines language, so every frontend gets the same guarantees.

Out of scope for this ADR, and expected to need more: Rust's discovery is a
walk of `mod` declarations from crate roots rather than a file list, and
Kotlin's Module is a package spanning files. Both fit `lower`'s whole-program
shape but may want `discover` to see more than a `GitIgnore`.

## Considered options

- **One opaque `pipeline` method.** Smallest class, and the user's first sketch.
  Rejected: every language would own the Command, Baseline and write-back, and
  the five language studies all needed the stages anyway.
- **Per-file `lower`.** Rejected: Haskell, Python, Nix and Kotlin cannot name a
  dependency without seeing the whole program.
- **Lints that report through `ReportProblem` and read the Baseline**, as
  `noRelativeSpecifiers` did. Rejected: it made each lint responsible for
  baseline-aware fixing, which is the driver's policy.
- **Running every detected language in a polyglot project.** Deferred: the
  Module graph would need edges across languages first.
- **Languages at the top of `src/`** (`TypeScript/`, `Haskell/`, ...). Rejected:
  the registry rule would have to name every language directory.

## Consequences

- `Deslop/**` imports nothing under `Frontend/**`, checkable with
  `grep -rn "^import Frontend" src/Deslop/`.
- `doWork` keeps its signature; `Command` moved to `Deslop.Command` and is
  re-exported from `Params`.
- Output changed in four places, all intended: a `TypeScript project detected.`
  line after the title; `NoFrontendDetected` instead of a missing-tsconfig
  error; neutral `--help` text; neutral Glob+ hints.
- Lowering now runs after every file is linted rather than per file. Same
  Modules, in the same order.
- `loadGitIgnore` takes the frontend's generated-directory predicate, so it
  still never walks `node_modules`.
