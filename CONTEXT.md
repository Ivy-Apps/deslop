# Deslop

A linter and auto-fixer for TypeScript codebases: it walks a project, reports
architectural and stylistic problems, and can fix the mechanical ones in place.

## Language

### Ignoring

**Entry**:
One item of a directory listing, together with whether it is a directory.
Directory-ness is part of the term because ignore rules distinguish `build/`
from `build`.
_Avoid_: File, node, item

**Ignore Pattern**:
The compiled glob from one `.gitignore` line, as path segments.
_Avoid_: Pattern (means two other things — see below), glob

**Ignore Rule**:
One meaningful line of a `.gitignore`: an Ignore Pattern plus its negation,
directory-only and anchored flags.
_Avoid_: Rule (means a Rulebook Rule), line, exclude

**Ignore Scope**:
A directory holding a `.gitignore`, plus that file's Ignore Rules in source
order. Governs only paths beneath itself.
_Avoid_: Gitignore file, ignore group

**GitIgnore**:
Every Ignore Scope in a project, ordered shallowest-first so that a deeper
`.gitignore` overrides a shallower one.

**Always-Ignored Directory**:
A directory never traversed for any reason — `node_modules`, `.git`, `dist` and
friends. Distinct from a gitignored path in that no Ignore Rule can re-include
it, and it is not even scanned for `.gitignore` files.
_Avoid_: Hardcoded ignore, skip list, excluded directory

### TypeScript configuration

**TsConfig**:
The effective TypeScript configuration a run resolves to: a Paths Base and its
path mappings, after the whole Extends Chain has been merged. Not a file.
_Avoid_: tsconfig.json (that is one file), compiler options

**TsConfig File**:
One `tsconfig.json` exactly as authored, holding only what it itself declares.
Several of them make one TsConfig.
_Avoid_: TsConfig, config

**Extends Chain**:
The TsConfig Files reachable from the project's root config through `extends`,
in precedence order: a file always outranks the ones it extends, and a later
entry of an array `extends` outranks an earlier one. Two branches reaching one
shared file is a diamond and is legal; a file reaching itself is an error.
_Avoid_: Inheritance, config hierarchy

**Paths Base**:
The directory a TsConfig Pattern's values resolve against. A declared `baseUrl`
wherever the Extends Chain has one, otherwise the directory of the TsConfig File
that declared the winning mappings.
_Avoid_: baseUrl (it is only sometimes one), project root, config dir

**Project Root**:
The directory the run was pointed at, and the base every path Deslop reports is
relative to. Not the Paths Base: a project whose aliases live in a `config/`
subdirectory has the two in different places.
_Avoid_: Paths Base, baseUrl, source root, working directory

### Modules and dependencies

**Module**:
One unit of code Deslop puts in the graph. In TypeScript that is one file, but
that is a fact about TypeScript rather than about Deslop: a Go package is a
directory and a Rust `mod` may have no file of its own.

**Module Id**:
Which Module this is. One per Module, minted by a language frontend from
whatever identifies a Module in that language - for TypeScript, the canonical
path of its file. Machine-specific, so it never reaches a report or a Baseline.
_Avoid_: Module Name (see Overloaded terms), path, key

**Module Name**:
What a Module is called, in the vocabulary Rulebook Rules and reports are
written in - `@/features/home`. A Module has several whenever several names
resolve to it, and a Glob+ Pattern matching any one matches the Module. Always
spelled with `/`, whatever separator the language itself writes - a Haskell
frontend mints `Data/List/NonEmpty` (ADR 19).
_Avoid_: Module Id, alias, import path

**Canonical Name**:
The one Module Name a report prints and a Rule Violation's Problem ID is built
from. Fixed per Module, so a Problem keeps its ID from run to run.
_Avoid_: Primary name, main alias

**Specifier**:
The text a source file actually writes to name what it depends on - `"@/a"`,
`"./helper"`, `"react"`. What the author typed, before anything resolves it.
Not a Module Name however alike the two look: `./helper` denotes a different
Module in every file that writes it. A built-in lint rule judges the Specifier;
a Rulebook Rule judges what it resolved to.
_Avoid_: Import path, module id, target

**Unscanned Module**:
A Module a Specifier resolved to that Deslop never read - gitignored, outside
the tree it was pointed at, or not a source file. It is a vertex like any other
and carries the names the frontend minted for it, but it has no dependencies of
its own, so a transitive chain stops there.
_Avoid_: External module (that is `react`, which resolved to nothing at all)

**Location**:
Where something was written and what the source says there: a Project Relative
Path, a 1-based line, and the statement verbatim. The line is for the reader
only and never enters a Problem ID.
_Avoid_: Position, span, source range

**Project Relative Path**:
A path spelled from the Project Root and from nothing else. Every path Deslop
reports or writes into a Baseline is one, which is what lets a Baseline be
committed and read back on another machine.
_Avoid_: Relative path (relative to what is the whole of its meaning)

**Dependency Edge**:
One Module depending on another: the Specifier as written, what it resolved to
(with every name that thing answers to), its Edge Kind, and its Location.
_Avoid_: Import (that is one kind of edge), link

**Edge Kind**:
Whether a Dependency Edge is private to the Module or part of the surface it
exposes - an Import or a Re-export. Both are edges and the graph treats them
alike; the distinction exists so a report's sentence matches the statement
quoted under it.

**Re-export**:
A Dependency Edge that also re-exposes what it depends on: TypeScript's
`export ... from`, Rust's `pub use`, a Haskell export list. Go and Kotlin have
no such construct.
_Avoid_: Export (a plain `export const` names no Module and is not an edge)

**Barrel**:
A Module whose purpose is to re-export others, conventionally
`<dir>/index.ts`. It answers to both its own name and its directory's, and
those are one Module.
_Avoid_: Index file (that is the file; the Barrel is what it means), facade

### Rules and problems

**Rulebook**:
A user-authored YAML file under `deslop/rules/` declaring architectural Rules
for a project.

**Rulebook Rule**:
One rule in a Rulebook: a target pattern plus `forbids` / `allows` / `uses` /
`exists` clauses. Unqualified "Rule" in this codebase means this one.

**Problem**:
Something Deslop reports — either a Lint Problem (from a built-in check like
`no-relative-imports`) or a Rule Violation (from a Rulebook Rule).

**Baseline**:
The set of Problem IDs in `deslop/baseline.yaml` that a project has accepted, so
they are suppressed from future checks. Committed, so every ID in it is
portable: paths are relative to the Project Root and spelled with `/`, and
nothing in one names the machine that wrote it.

**Auto-Fixable**:
A Problem that `deslop fix` can resolve without human input. Only some Lint
Problems are; a Rule Violation never is, because a Rulebook Rule describes
architecture rather than a rewrite.
_Avoid_: Fixable (every Problem has a suggested fix; only some are automatic)

**Hop**:
One Dependency Edge of the shortest path from a module to a module it reaches
transitively. A direct dependency is 1 hop. A Re-export is a hop like any
other, which is what lets a chain run through a Barrel.
_Avoid_: Step, level, depth, degree

### Glob+

**Glob+ Variable**:
A named part of a path that a Rulebook Rule captures from its target and reuses
in its clauses. Written `{{provider-name}}`.
_Avoid_: Casing variable (the casing is how it is written, not what it is),
placeholder, token

**Variable Name**:
The identity of a Glob+ Variable, as kebab-case words. One name has four
spellings - `{{ProviderName}}`, `{{providerName}}`, `{{provider-name}}`,
`{{PROVIDER_NAME}}` - and all four are the same variable.
_Avoid_: Key, label

**Casing**:
Which of the four spellings a Glob+ Variable is written in at one occurrence.
Inferred from the spelling itself; never declared separately.
_Avoid_: Case style, format

**Binding**:
A Variable Name together with its captured value in all four Casings. A Rulebook
Rule's bindings are produced by matching its Target Pattern and consumed by its
Clause Patterns.
_Avoid_: Binding value, capture (a capture is one regex group; a binding may
come from several)

**Spelling**:
One way a name can be written in a Casing. kebab-case and CONSTANT_CASE give a
name exactly one spelling; PascalCase and camelCase give it several, because any
word may be written as an acronym - `db-connection` is spelled `DbConnection` and
`DBConnection`. Two occurrences of one Glob+ Variable bind when some name spells
both, which is a different question from what either one decodes to.
_Avoid_: Rendering, form, variant

**Polarity**:
The direction Deslop errs in when a Glob+ Variable's Spelling has to be guessed:
**Widen** accepts every Spelling of every name the capture could denote, **Narrow**
accepts only the canonical one. Chosen per clause so that a wrong guess costs a
false positive rather than a false negative - `target` and `forbids` widen, while
`allows`, `uses` and `exists` narrow, because those are the clauses where a match
*silences* a report. An Exclude Pattern has no Polarity: it has no variables to
guess about.
_Avoid_: Direction, sign, mode, Forbidding/Requiring (the previous names)

**Path Segment**:
One `/`-delimited part of a module id, and the unit a Glob+ Pattern matches
against. `@/components/stripe/View` is four segments.
_Avoid_: Path component, part, directory (a segment may be a file)

**GlobStar**:
`**`: zero or many whole Path Segments. It is always a whole segment of the
pattern - `**` glued to text inside a segment is not a GlobStar and does not
compile.
_Avoid_: Wildcard (that is `*`), double star, recursive glob

**Parent Dir**:
`..`: a whole segment of a Clause Pattern that cancels the segment to its left,
so a clause can name a directory relative to `{{TARGET_DIR}}`. It may only cancel
a segment whose text the pattern determines - never a GlobStar, never a segment
containing `*`. With nothing left to cancel it does nothing. It is a whole
segment or it is nothing: `..foo` is ordinary text.
_Avoid_: Dot-dot, up, back, relative segment, parent path

**Anchored Variable**:
A Glob+ Variable whose Path Segment is fixed by the pattern, because no GlobStar
stands on one of its sides. Only Anchored Variables are allowed in a Target
Pattern: an unanchored one has no defined meaning, since the path rather than the
pattern would decide which segment it names.
_Avoid_: Pinned variable, positional variable

**Rulebook Compiler**:
The stage that turns a raw Rulebook, as authored in YAML, into a valid Rulebook
whose patterns are compiled - or into the collected Compilation Errors explaining
why it cannot. Every Rulebook reaching enforcement has been through it.
_Avoid_: Parser (parsing is one part of it), validator, loader (loading is
reading bytes)

**Compilation Error**:
One reason a raw Rulebook cannot become a Rulebook, naming the file, the Rule and
the field it came from. A run reports all of them at once and enforces nothing.
_Avoid_: Problem (that is something Deslop reports *about a codebase*), parse
error

**Target Pattern**:
The Glob+ pattern in a Rule's `target`. The only pattern that *captures*
variables, and the one that decides which variables its clauses may use. Cannot
contain `{{TARGET_DIR}}`, which is derived from what it matches, nor a Parent
Dir, since it is matched against whole module ids.

**Clause Pattern**:
A Glob+ pattern in `forbids` / `allows` / `uses` / `exists`. *Substitutes*
variables rather than capturing them, and may use `{{TARGET_DIR}}` and a Parent
Dir. May only name variables bound by its own Rule's Target Pattern.

**Exclude Pattern**:
A Glob+ pattern in a Rule's `exclude`. A plain glob: it filters the target and
binds nothing, so it may contain neither variables nor a Parent Dir.

### Benchmarking

**Reference**:
The saved measurements in `bench/reference.yaml` that a benchmark run is judged
against, together with the environment they were taken under.
_Avoid_: Baseline (means the accepted Problem IDs, and is also one of the groups
the benchmark measures — see below), snapshot, budget

### Overloaded terms

**Pattern** is deliberately never used unqualified. Three distinct things carry
the word, and each has its own syntax and matching rules:

- **Glob+ Pattern** (`Deslop.GlobPlus`) - a glob over module ids, supporting
  named variables. Used by Rulebook Rules. Itself three kinds, which differ in
  what they may contain: Target Pattern, Clause Pattern and Exclude Pattern
  (see Glob+ above).
- **TsConfig Pattern** (`TypeScript.Config.Pattern`) — a `tsconfig.json` path
  mapping, either `Exact` or a single-`*` `Wildcard`.
- **Ignore Pattern** (`Git.Ignore.IgnorePattern`) — a `.gitignore` glob, per
  gitignore(5).

**Module Id**, **Module Name** and **Specifier** all name a module. Only one of
them identifies it, and only one of them is a name:

- A **Module Id** identifies. One per Module, opaque to everything outside the
  language frontend that minted it, and never rendered - it names this machine,
  so a Problem ID or a Baseline entry built from one would match nothing
  anywhere else.
- A **Module Name** names. A Module has as many as there are ways to name it:
  a Barrel's directory and index forms, plus every alias resolving to it. These
  are what Rules match and what reports print.

- A **Specifier** is neither. It is what one file typed. Usually it looks
  exactly like a Name - `@/features/home` is both - and that coincidence is why
  the two were one type for so long. `./helper` is the case that breaks it.

A pattern matches a Module when *any* of its Names does, so these must not be
confused: matching on the Id would make the same Module answer under one
spelling and not another, and matching on a Specifier would make a Module's
identity depend on who imported it.
