# 17. A module is identified by what it resolves to, not by what an import writes

Date: 2026-09-07

## Status

Accepted. Refined by
[ADR 19](0019-module-names-are-slash-segmented-in-every-language.md), which
settles how a name is spelled in a language that does not use `/`.

## Context

A vertex in the module graph was keyed by **text**. A module's own key came
from reverse-resolving its file path; an edge's key was the specifier the
author had typed. These are two different functions, and they disagree, so one
file became several vertices - all but one of them a sink with no outgoing
edges, indistinguishable from `react`.

Every disagreement is a dropped chain, and the report says nothing about it:

- **A barrel is two vertices.** `reverseResolveImport` deliberately kept the
  written form when the resolved form was `target <> "/index"`, so the file
  `src/a/index.ts` was the vertex `@/a/index` while every `import from "@/a"`
  pointed at a different, edgeless `@/a`
  ([#204](https://github.com/Ivy-Apps/deslop/issues/204)).
- **Two aliases are two vertices.** `@/lib/x` and `@lib/x` name one file and
  became two ids ([#117](https://github.com/Ivy-Apps/deslop/issues/117)).
- **Re-exports were not edges at all**, so a barrel had no outgoing edges to
  begin with ([#173](https://github.com/Ivy-Apps/deslop/issues/173)).
- **The baseline changed the graph.** The AST was built from the CST *after*
  the lint pass rewrote it, so accepting a `no-relative-imports` Problem left a
  raw `./helper` as an edge key and truncated every chain through it.
  `fixtures/ts-project-1` did exactly this, hiding a real violation.

All four are one defect: a `forbids: transitive: true` rule stops checking
partway and CI stays green, which is worse than having no rule, because a
reviewer believes the boundary is enforced.

Three models were considered.

1. **Canonicalise to the `/index` form.** Drop the special case so an edge
   written `@/a` becomes `@/a/index`. Cheapest, and chains survive barrels. But
   `target: "@/a"` in a rulebook still matches nothing, two aliases are still
   two vertices, and the graph still takes its edges from the lint pass.
2. **Register an index file under its directory form.** Makes `target: "@/a"`
   work. But `import "@/a/index"` - legal, and written in real code - becomes
   the new phantom. The bug moves rather than disappearing.
3. **Identity is the resolved file.**

## Decision

**A module is identified by what its specifier resolves to. One file, one
vertex.**

Identity, name and what an author wrote are three separate types in
`Deslop.Module`:

- **`ModuleId`** identifies. One per module, minted by a language frontend and
  never interpreted by the core, which needs only `Eq` and `Ord` to key a graph
  by it. It is machine-specific and is never rendered - no `Renderable`
  instance, and nothing puts one in a report or a Baseline.
- **`ModuleName`** names. A module has several: a barrel answers to both
  `@/features/home` and `@/features/home/index`, and every alias mapping to the
  file adds another. Rules and reports speak in these.
- **`Specifier`** is what a file wrote. A fact about one file, and not a name:
  `./helper` denotes a different module in every file that writes it, and a
  name has to mean the same thing to every reader. The two answer different
  questions, which is why an edge carries both -
  `TypeScript.Lint.RelativeSpecifiers` judges what was *written* and a Rule's
  `forbids` judges what was *reached*.

A pattern matches a module when **any** of its names matches; a Target
Pattern's captures come from the first name that matched, which is the
canonical one whenever it matched at all. The canonical name is unchanged from
what `reverseResolve` returned before, so **no existing Baseline entry
changes**.

`TypeScript.Module` becomes the only place a specifier is resolved, which is
what makes the graph independent of the lint pass and therefore of the Baseline.

**The frontend also names what it resolved.** An edge's resolved target carries
the `ModuleName`s of the thing it points at, rather than the core deriving them.
Naming an import path is the most language-specific operation there is - a Go
frontend answers "one import path", a Rust one "the crate path plus every
`pub use` alias" - and the only material the core could have invented one from
is the specifier, which is not a name. `Deslop.CodeGraph` used to name anything
it had no module for by the smallest specifier that reached it, and `.` sorts
below `@`, so a gitignored file reached as both `../shared/x` and `@/shared/x`
was reported under the one spelling that is not a name at all.

A vertex is therefore one of three things, and they are kept apart because only
the first can be followed any further:

| | what it is | named by |
|---|---|---|
| `ParsedModule` | lowered by a frontend | its own names |
| `UnscannedModule` | resolved, never read: gitignored, outside the scanned tree, or not a source file | the names the edge carried |
| `ExternalModule` | `react`: nothing in the project answers to it | its specifier |

**Nothing machine-specific crosses the seam.** A `Module`'s path is a
`ProjectRelativePath`, minted by the frontend against the root it already holds,
so `Deslop/` carries no host path at all except the deliberately opaque
`ModuleId`. P17 pins it.

**An edge says where it was written.** A `Location` carries a file, a 1-based
line and the statement verbatim, and a `ViolationKind` carries one on the two
branches that have a statement to point at. `MissingUse` and `MissingModule`
carry none, because the complaint is that nobody wrote one; a single optional
location would have had to mean both "this kind never has one" and "this one
happens not to". The line is derived by counting newlines across the CST, whose
rendering is byte-for-byte lossless, and it is for the reader only: it never
enters a `ProblemId`, or adding a blank line at the top of a file would
unsuppress every accepted Problem below it. P15 pins that.

The core stays language-agnostic because `ModuleId` is opaque. "A module is a
file" is a TypeScript claim and a false one elsewhere: a Go package is a
directory of files, and a Rust `mod` may have no file at all. A Go frontend
mints its `ModuleId` from a package directory and nothing in `Deslop/` changes.

A re-export is an edge like any other, distinguished only by
`EdgeKind = ImportEdge | ReExportEdge`, which exists so a report can say
"re-exports" above a quoted `export` statement rather than "imports". The
distinction is real in Rust (`pub use`) and Haskell (an export list) too; Go
and Kotlin have no such construct and mint only `ImportEdge`.

## Consequences

`target: "@/features/home"` now matches the barrel, `exists` no longer passes
vacuously on a name nothing on disk answers to, and cycles through a re-export
are found.

Chains that were being silently truncated now report. `fixtures/ts-project-1`
gains one violation that the Baseline had been hiding - a true positive, and
the reason P12 in `TypeScript.Lint.RelativeSpecifiersPropSpec` exists.

Properties P7-P12 are the specification: P9 (a re-export is an edge), P10 (a
barrel's two spellings are one module) and P11 (two aliases are one module)
each fail against a text-keyed graph, and P12 pins the Baseline's independence
from the graph.

**Accepted limitations.** A symlinked *file* and a case-insensitive filesystem
can still split one file into two vertices, because identity is the
canonicalised path and neither is canonicalised away. Both were already broken,
neither is worth the machinery, and both are noted here rather than fixed.

An `UnscannedModule` has no outgoing edges, so a `transitive: true` chain stops
there and says nothing about it - the same class of defect this ADR is about, in
its last remaining form. `ParsedModule`/`UnscannedModule`/`ExternalModule` are
separate constructors so that a report *can* say which happened;
`fixtures/ts-unscanned-project` is the case, and making the report say it is
[#225](https://github.com/Ivy-Apps/deslop/issues/225).

`TypeScript.Module` lowers the CST *after* the lint pass has rewritten it, so a
violation's quoted statement is the fixed spelling while its line points at the
unfixed one on disk. Pre-existing, and newly visible now that a report carries a
line. Also #225.
