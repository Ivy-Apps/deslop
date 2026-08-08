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

### Overloaded terms

**Pattern** is deliberately never used unqualified. Three distinct things carry
the word, and each has its own syntax and matching rules:

- **Rulebook Pattern** (`Deslop.GlobPlus.Pattern`) — a glob over module ids,
  supporting `{{FileName}}` casing variables. Used by Rulebook Rules.
- **TsConfig Pattern** (`TypeScript.Config.Pattern`) — a `tsconfig.json` path
  mapping, either `Exact` or a single-`*` `Wildcard`.
- **Ignore Pattern** (`Git.Ignore.IgnorePattern`) — a `.gitignore` glob, per
  gitignore(5).
