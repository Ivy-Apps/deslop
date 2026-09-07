# 19. Module names are slash-segmented in every language

Date: 2026-09-07

## Status

Accepted. Refines [ADR 9](0009-glob-plus-matches-path-segments.md), which
established that a Glob+ pattern is a list of path segments, and
[ADR 17](0017-a-module-is-identified-by-what-it-resolves-to.md), which
established what a Module Name is.

## Context

`Deslop.GlobPlus.segmentsOf` is `Segments . T.splitOn "/"`. The core - both the
matcher and the pattern compiler - takes `/` to be what separates one part of a
module's name from the next.

That is a fact about TypeScript, and about Go, and about nothing else Deslop
intends to support. Haskell writes `Data.List.NonEmpty`, Python `pkg.sub.mod`,
Kotlin and Java `com.foo.Bar`, and Rust `crate::foo::bar`. A `paths` alias in a
`tsconfig` happens to be slash-shaped; a package name in four of the six target
languages is not.

Three models were considered.

1. **A separator carried per language.** A `Language` value in the reader
   environment, read at two sites: minting a Module Name's segments, and
   compiling a Rulebook pattern. Names and patterns then keep each language's
   own spelling, so a Haskell report says `Deslop.Rule.Enforcer` and a Haskell
   rulebook says `Deslop.Rule.**`. It costs an ambient value that the compiler
   and the matcher must agree on, and it is one more thing a frontend author has
   to get right.
2. **A structurally segmented `ModuleName`**, carrying `NonEmpty Text` rather
   than text. The core could then never split anything twice or differently.
   But a Rulebook pattern still arrives as text out of a YAML file and still has
   to be split by *someone's* separator, and rendering a name back for a report
   needs one too. The ambient value returns, and the duplication with it.
3. **Frontends normalise.**

## Decision

**A Module Name is spelled with `/`, whatever the language writes.** A Haskell
frontend mints `Data/List/NonEmpty`, a Python one `pkg/sub/mod`, a Kotlin one
`com/foo/Bar`, a Rust one `crate/foo/bar`. `Deslop.GlobPlus` is unchanged, and
so is every Rulebook already written.

Rules read better this way. A Rulebook is about architecture, and architecture
is about directories: `Deslop/Rule/**` says the same thing to a reader of any of
the six languages, and in Haskell, Python, Java and Kotlin it is also the layout
on disk, because those languages mirror their package structure into
directories.

It is written down here because it is not discoverable from the code. Nothing in
`Deslop/` says "and a Haskell frontend must convert `.` to `/`" - the split is
one line in a matcher, and the next person adding a language would reasonably
mint `Data.List.NonEmpty` and find their patterns silently matching one segment
where they meant three.

## Consequences

A frontend for a language that does not use `/` owes one conversion, in both
directions: `/` when it mints a Module Name, and back again if it ever needs to
speak the language's own spelling. Deslop never sees the second form.

Reports print the slash form. `Module 'Deslop/Rule/Enforcer' directly imports
'TypeScript/AST'` is not how a Haskell programmer writes that module's name, and
that is the price. It is charged only to languages Deslop does not yet support,
and it buys every rule in every language being written the same way.

Rust is the one target where the slash form is not also the layout on disk - a
`mod` may live anywhere, or nowhere. Its names are still slash-segmented; they
simply describe the module tree rather than the file tree.

This is revisitable. Option 1 above remains open, and the seam it would need is
small: `segmentsOf` and the pattern compiler are the only two places that split.
Nothing decided here makes that harder later, and a real second frontend is
better evidence for the design than an imagined one.
