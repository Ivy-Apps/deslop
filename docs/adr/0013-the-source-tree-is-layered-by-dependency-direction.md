# 13. The source tree is layered by dependency direction

Date: 2026-08-23

## Status

Accepted

## Context

Deslop is meant to be language-agnostic at its core, with TypeScript as the
first of several frontends. The tree did not say so, and in four places it said
the opposite.

`ModuleId` lived in `TypeScript.ModuleResolver`. Seven modules under `Deslop.`
imported it from there, so the "language-agnostic core" could not be compiled
without TypeScript. `Deslop.AST` held both the target vocabulary and `parseAst`,
the function that turns a TypeScript CST into it, which meant the one module
that defines what a language must produce was itself written against one
language. `Deslop.Lint.RelativeImports` rewrote `TsProgram` nodes and resolved
`tsconfig` path aliases while sitting in the core beside `CycleDetection`, which
reads nothing but the `ModuleGraph`.

Three modules had grown into catch-alls. `Types` held a rendering class, the
top-level error type, and the run-outcome types, related only by having nowhere
else to go. `UI` mixed ANSI primitives with the sentences Deslop says, and
because `Effects.CLI` imported it for the colours, the CLI effect dragged
`Deslop.Problem` and the problem formatter behind it. `Effects.FileSystem` owned
both the filesystem effect and the path types, so `Deslop.Problem` and
`TypeScript.CST` - both pure - imported an effect module to name a path.

Naming had drifted too. `Deslop.Rulebook`, `Deslop.Rulebook.Dto`,
`Deslop.Rulebook.Loader` and `Deslop.Rulebook.Compiler` were one directory while
`Deslop.RuleEnforcer`, which consumes them, was a sibling of the whole group.
Specs had names their modules did not: `TypeScript.RenderableSpec` tested a
module that does not exist, `Deslop.ASTSpec` tested `parseAst`, and one root
`describe` read `Deslop.GlobPLus`.

None of this was breaking anything. It was making every future feature slightly
harder to place, which is the cost that compounds.

## Decision

**The tree has four layers, and imports only ever point inward.**

```
Deslop.hs          orchestration - the only module that knows both a language and the core
  └─ Deslop/       the language-agnostic core: AST, CodeGraph, Problem, GlobPlus, Rule
       └─ TypeScript/    a language frontend: bytes → Tokens → CST → Deslop.AST
            └─ Effects/  FileSystem/  Git/  Utils  Renderable    infrastructure
```

`Deslop/**` contains no `import TypeScript` line, and the compiler is what
enforces that, not a convention. A second language is added by writing a
`<Lang>/` directory that ends in a `<Lang>.AST` producing `Deslop.AST`.

**`Deslop.AST` is the seam.** It holds `ModuleId`, `AstNode` and `AstModule` and
nothing else - no IO, no language. `TypeScript.AST` holds the lowering
`TsProgram -> AstModule`. `TypeScript.ModuleResolver` imports `ModuleId` rather
than defining it: what varies per language is how a written import resolves to a
module, not what a module is.

**A built-in Rule lives with what it reads.** `Deslop.Rule.Lint.CycleDetection`
reads only the `ModuleGraph`, so it is core. `TypeScript.Lint.RelativeImports`
rewrites `TsProgram` and reads `tsconfig` aliases, so it is TypeScript's. There
is deliberately no registry abstraction over the two; `Deslop.hs` calls each by
name, and that is the honest amount of machinery for two rules.

**One module per stage, one concept per module.** `Types` is dissolved into
`Renderable`, `Deslop.Error` and `Deslop.RunReport`. `UI` becomes pure `Text`
composition and the ANSI primitives move into `Effects.CLI`, whose interpreter
is now the only code in Deslop that writes to a terminal. The path vocabulary
moves to `FileSystem.Path`, leaving `Effects.FileSystem` with the effect alone,
so a pure module can name a path without taking on an effect - the same fix
[ADR 10](0010-rulebook-compilation-is-a-separate-stage.md) made for
`Deslop.Rulebook`.

**Rules get one namespace.** `Deslop.Rule.Book{,.Dto,.Loader,.Compiler}` is the
rulebook artifact and its pipeline; `Deslop.Rule.Enforcer` and
`Deslop.Rule.Lint.*` are enforcement. The type is still spelled `Rulebook` -
that is the word users read - and only the module path changed.

**`Effects/` holds every effect declaration and its interpreter**, including
`Effects.ReportProblem`, which names `Deslop.Problem`. This is the one place the
inward-pointing rule is knowingly broken, and it buys a single directory that
answers "what can this program do to the outside world".

### The test tree

**A spec lives where its module lives, and its root `describe` names that
module.** `src/Deslop/Problem/Baseline.hs` is tested by
`test/Deslop/Problem/BaselineSpec.hs`, whose root `describe` is
`"Deslop.Problem.Baseline"`. That is what makes `nix run .#test -- Deslop.GlobPlus`
select what its name says. Variants suffix the module name:
`GlobPlusPropSpec`, `GlobPlusSemanticsSpec`.

`test/E2E/` is the one exemption: those specs exercise a pipeline rather than a
module, and name the scope instead - `E2E.File`, `E2E.Project`.

**A helper is shared only once it crosses a file.** A generator or fixture used
by one spec stays private in that spec. When a second spec needs it, it moves to
`Fixtures.<the.full.module.path>` or `Generators.<the.full.module.path>` -
mirroring the path so `Fixtures.TypeScript.Config` can never be mistaken for a
fixture of some other `Config`. `TestUtils` keeps only what is domain-free:
golden helpers, fixture-directory access, and assertion combinators.

Applying that rule found that Deslop has no cross-file generators at all. The
GlobPlus specs each hold their own `genWord`, `spell` and `allCasings`; the names
coincide but nothing is imported. `test/Generators/` therefore does not exist
yet, and `Deslop.GlobPlus.Oracle` remains what it always was: the one generator
module more than one spec depends on.

**Fixture projects moved from `test/fixtures/` to `fixtures/`.** Not cosmetic:
`test/Fixtures/` and `test/fixtures/` are the same directory on a
case-insensitive filesystem, so the fixture modules could not coexist with them
under `test/`.

## Considered options

- **Leave `ModuleId` in `TypeScript.ModuleResolver` and revisit when a second
  language lands.** Strictly move-only, smallest diff. Rejected because it is the
  single edge that decides whether the core compiles without TypeScript, and
  moving a ten-line newtype now is cheaper than moving it once three languages
  reference it.
- **Widen `Deslop.AST` to export the `ModuleId` constructor**, so
  `TypeScript.ModuleResolver` could keep pattern-matching it. Rejected: that
  discards the `moduleIdUnsafe` discipline the type already had, in exchange for
  not rewriting four expressions.
- **Keep `Types.hs`.** It is in the issue's proposed tree and costs nothing today.
  Rejected: a module named for its syntactic category rather than its subject has
  no criterion for what belongs in it, so it only ever grows.
- **Disperse `UI`'s text into the domain modules** - `humanReadable` to
  `Deslop.Error`, `summaryLine` to `Deslop.RunReport`. More DDD-pure, and it would
  have removed `UI` entirely. Rejected because it scatters the user-facing copy
  across four modules; keeping Deslop's voice in one file is worth one import.
- **Move `Effects.ReportProblem` to `Deslop.Problem.Reporter`**, making
  `Effects/` free of domain imports and the layering rule exception-free. Rejected
  narrowly: reading the whole effect stack in one directory is worth more than a
  rule with no asterisks.
- **Both built-in lints under `Deslop.Rule.Lint`**, as the issue proposed.
  Rejected: it would put a module that rewrites `TsProgram` in the layer that is
  supposed to have never heard of TypeScript, which is the exact claim this ADR
  exists to make true.
- **Flat `Deslop.Rule.BookDto`** rather than `Deslop.Rule.Book.Dto`. Rejected:
  `Loader` and `Compiler` read as generic when they are rulebook-specific, and
  ADR 10's four-stage pipeline is easier to see as one directory.
- **Hoist every generator into a `*Generators` module** for uniformity. Rejected
  after counting: it would publish roughly 250 private helpers, none of which any
  other file imports, and leave each spec unable to be read on its own.
- **A registry for built-in Rules**, so `Deslop.hs` would not name each lint.
  Rejected as premature - there are two.

## Consequences

- `Deslop/**` has zero `import TypeScript` lines, checkable with one `grep`.
- `Effects.CLI` no longer transitively imports `Deslop.Problem`; `UI` performs no
  IO and is testable as pure text.
- `TypeScript.ModuleResolver.resolve` and `isRelativeImport` go through
  `ModuleId`'s accessor instead of its constructor. Same behaviour.
- `Deslop.Rule.Lint.CycleDetection` reads a `ProjectRoot` rather than a
  `TsConfig`. `Deslop.hs` supplies `ProjectRoot cfg.baseUrl`, which is the value
  it was already using. `baseUrl` is not really the project root - `Params.projectPath`
  is - but conflating them is a pre-existing bug and stays one, tracked separately.
- Four golden files changed, all mechanically: three re-wrapped because
  `ppShow` no longer needs a line break for the shorter fixture path, and one
  because an Aeson parse error quotes the DTO's module name, now
  `Deslop.Rule.Book.Dto`. No golden changed because Deslop said something
  different.
- `TestUtils` lost three exports that nothing referenced: `failBeatiful`,
  `requireEnvVar` and `projectFixturePath`.
- Anything that hard-codes `test/fixtures` - a script, a bookmark, a stale
  branch - now wants `fixtures`.
