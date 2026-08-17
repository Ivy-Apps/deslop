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
they are suppressed from future checks.

**Auto-Fixable**:
A Problem that `deslop fix` can resolve without human input. Only some Lint
Problems are; a Rule Violation never is, because a Rulebook Rule describes
architecture rather than a rewrite.
_Avoid_: Fixable (every Problem has a suggested fix; only some are automatic)

**Hop**:
One edge of the shortest import path from a module to a module it reaches
transitively. A direct import is 1 hop.
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
contain `{{TARGET_DIR}}`, which is derived from what it matches.

**Clause Pattern**:
A Glob+ pattern in `forbids` / `allows` / `uses` / `exists`. *Substitutes*
variables rather than capturing them, and may use `{{TARGET_DIR}}`. May only
name variables bound by its own Rule's Target Pattern.

**Exclude Pattern**:
A Glob+ pattern in a Rule's `exclude`. A plain glob: it filters the target and
binds nothing, so it may not contain variables at all.

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
