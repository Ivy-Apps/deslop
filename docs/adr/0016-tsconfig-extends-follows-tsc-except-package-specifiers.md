# 16. `tsconfig` `extends` follows tsc, except for package specifiers

Date: 2026-09-06

## Status

Accepted

## Context

Deslop read exactly one file, `<project>/tsconfig.json`, and took `baseUrl` and
`paths` from it. Any project whose aliases live in a config it *extends* -
which is every mono-repo laid out the way TypeScript documents
([#58](https://github.com/Ivy-Apps/deslop/issues/58)) - looked to Deslop like a
project with no aliases at all, so every import came back unresolved.

`extends` is not a merge in the ordinary sense. Three of its rules decide
whether Deslop agrees with the compiler about what a project's aliases are, and
two of them surprise people:

- `compilerOptions` are overlaid key by key, and `paths` is **one key**. A
  config that declares any `paths` discards its base's entirely. Teams
  routinely expect a union.
- A relative `baseUrl` is made absolute against the directory of the file that
  *declared* it, not against the root config's.
- When no config in the chain declares a `baseUrl`, `paths` values resolve
  against the directory of the config that declared `paths` - what the compiler
  tracks as `pathsBasePath`. So a base config in a `config/` subdirectory
  resolves its own aliases relative to `config/`.

An `extends` value is a path when it is explicitly relative or rooted, and a
Node package specifier otherwise - `@repo/typescript-config/base.json`, which
is what a Turborepo workspace writes.

## Decision

**Deslop resolves the chain exactly as tsc does, except that it does not
resolve package specifiers.**

Exactly means all three rules above, including the two surprising ones. The
alternative - unioning `paths` across the chain - is worse than merely
non-standard: it resolves aliases the compiler rejects, so Deslop would fall
silent about imports that are genuinely broken. Where the two directions of
error are not symmetric, the safe one is to agree with the compiler.

The merge is a right-biased `Semigroup` on `TypeScript.Config.Declared`, one
value per file with that file's own directory already baked in. `paths`
replacing rather than unioning is not special-cased anywhere: it is what `Last`
does. Associativity, identity and last-declaration-wins are property-tested,
because the instance is written by hand and every field of it can be flipped
the wrong way round without the types noticing.

**A package specifier is skipped with a warning, not an error.** Deslop reads
two options out of a `tsconfig`, and a *shared* config package essentially never
declares `paths` - aliases are relative to the app that owns them. Refusing to
run would break every Turborepo workspace over a file that, in practice, holds
strictness flags Deslop does not read. The warning names the specifier and the
file, so a project that does keep its aliases there learns why they look
unresolved instead of drowning in false problems.

Everything else in the chain is fatal: a base that is not there, a base that
does not parse, a cycle, and an `extends` that is neither a string nor an array
of strings. Those are forms we claim to handle, and a chain we half-understand
produces wrong verdicts rather than no verdict.

Cycles are detected against the **in-progress chain**, not the set of every file
seen. Two branches extending one shared base is a diamond, and legal; a
visited-set would reject it.

## Considered options

- **Union `paths` across the chain.** Matches what people expect and lets a
  base config contribute aliases an app also extends. Rejected: it makes Deslop
  resolve imports `tsc` reports as errors, which converts real breakage into
  silence. It would also make the per-file resolution base load-bearing, since
  mappings from different files would then resolve against different
  directories.
- **Resolve package specifiers too**, by walking parent directories for
  `node_modules/<specifier>`. Roughly thirty lines, and it would cover both
  real idioms (`@repo/typescript-config/base.json`, `@tsconfig/strictest/tsconfig.json`).
  Rejected for now: it buys nothing Deslop reads today, and the warning makes
  the gap visible. It is additive - no model changes when it lands.
- **Full Node resolution**, parsing the target `package.json` and honouring
  `exports` conditions. Exact, and a conditional-exports matcher this codebase
  otherwise has no use for.
- **Fail on a package specifier**, for tsc parity. Rejected: Deslop would
  refuse to run on the standard Turborepo layout over a config whose contents
  it does not read.
- **Always resolve `paths` against the root config's directory** when no
  `baseUrl` is declared. Simpler fold, and identical to tsc whenever the base
  sits in the project root. Rejected: it is knowingly wrong for a base config in
  a subdirectory, which is exactly the mono-repo shape this work exists to
  support - and the correct rule costs one extra field in the fold.
- **Give every `PathMapping` its own base.** The general model, and the naive
  reading of "relative to the file they originated in". Rejected: with `paths`
  replaced rather than unioned, that field is provably constant across every
  mapping in a run.

## Consequences

- `TypeScript.Config` splits three ways, mirroring `Deslop.Rule.Book`:
  `Config.Dto` is the authored shape and the JSONC stripping, `Config.Loader` is
  the only part that touches IO, and `Config` keeps the domain type and the
  fold.
- `TsConfig.baseUrl` becomes `TsConfig.pathsBase`. In three of its four cases
  the value is not a declared `baseUrl`, and the field is read for exactly one
  purpose: the directory a `ValuePattern` resolves against.
- `DeslopError` loses `TsConfigNotFoundError` and `TsConfigParseError` for a
  single `TsConfigError Text`. The loader renders its own failures, as
  `Deslop.Rule.Book.Loader` already does, because they name files that only it
  knows about and `Deslop/` may not import `TypeScript`.
- `ts-cycles-project`'s config is split into `tsconfig.json` and
  `tsconfig.base.json` and its goldens are byte-identical, which is the
  invariance the fold has to have: splitting a config into base and child
  changes nothing.
- `ts-monorepo-project` pins the whole story end to end - an array `extends`, a
  base in a `config/` subdirectory declaring `paths` with no `baseUrl`, and
  cross-package aliases that `deslop fix` writes back.
