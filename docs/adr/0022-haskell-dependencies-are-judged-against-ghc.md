# 22. Haskell dependencies are judged against GHC

Date: 2026-10-24

## Status

Accepted. Applies [ADR 18](0018-typescript-resolution-is-judged-against-tsc.md)
to the Haskell frontend of
[ADR 21](0021-haskell-is-read-as-one-cabal-package.md).

## Context

The Haskell frontend decides two things a wrong answer to would make silent:
where an import is - layout, comments, pragmas, strings, the dash rule - and
which file it names. An import the lexer swallows is a dependency no Rule ever
sees, and a run that passes on it has passed for no reason.

GHC answers both questions with `ghc -M`, which prints the dependencies
between a project's source files as a Makefile. That is a real oracle, as
`tsc --traceResolution` is for TypeScript, but running it needs a compiler in
the test suite.

## Decision

**GHC's answers are recorded once and committed; the suite tests against the
recording**, exactly as ADR 18 does for `tsc`.

`fixtures/haskell/ghc-corpus.json` holds hand-authored cases - which files exist and
their literal contents, all in one library whose source directory is `src` -
each with a `ghc` field that is *only* ever written by
`nix run .#update-ghc-corpus`. That app runs `just update-ghc-corpus` in the
`ci` shell, whose `ghc` is the compiler the project builds with, with node on
PATH for the recorder. The recorder materialises each case in a temp
directory, runs `ghc -M -isrc` over its `.hs` files, and records every
`importer.o : imported.hi` (or `.hi-boot`) line as an `[importer, imported]`
pair of source files. A module's dependency on its own boot file is dropped:
it is GHC's bookkeeping, not an import.

`Frontend.Haskell.GhcCorpusSpec` runs each case through every stage of the
frontend over the in-memory filesystem and compares the set of
`(importer, imported)` pairs its `Resolved` edges name. Neither `ghc` nor node
is needed to run the suite.

**A case Deslop does not yet read the way GHC does carries a `knownGap` and is
asserted to still differ**, so closing a gap turns its own test red.

## Consequences

- `ghc -M` judges only what lies between two files of the project. External
  modules, re-export edges and how often a file is imported are not compared;
  the unit and property specs cover those.
- `ghc -M` stops reading at the first declaration that is not an import, as
  the frontend does, so the corpus pins that too.
- Writing it found a real bug: a string ending in a backslash at the end of a
  line made the lexer fail, and the whole file fell back to one opaque token
  with no imports.
- As with `.golden` and the tsc corpus, a failing corpus test is not a reason
  to re-record: a diff is a claim about what the compiler does.
