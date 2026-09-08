# 18. TypeScript resolution is judged against tsc

Date: 2026-09-07

## Status

Accepted

## Context

Deslop's correctness rests on resolving a specifier to the same file
TypeScript would. Everything above it - the module graph, transitive `forbids`,
`uses`, cycles - is only as right as that one answer, and a wrong answer is
invisible: an unresolved specifier looks exactly like a third-party package.

Our own tests could only ever restate our own beliefs. `tsc --traceResolution`
prints what the compiler actually decided, which makes a real oracle available
for the one question where being wrong is silent
([ADR 17](0017-a-module-is-identified-by-what-it-resolves-to.md) is what made
the question load-bearing).

The cost of using it is a Node toolchain, which a Haskell project does not
otherwise want: in the dev shell it is closure CI would realise for nothing,
and in the test suite it is a network-shaped dependency in a suite that is
otherwise hermetic and runs in milliseconds.

## Decision

**The compiler's answers are recorded once and committed; the suite tests
against the recording.**

`fixtures/resolution-corpus.json` holds hand-authored cases - the tsconfig
files, which files exist, and which specifier is written where - each with a
`tsc` field that is *only* ever written by
`nix run .#update-resolution-corpus`. That app puts node and typescript on
PATH, materialises each case in a temp directory, runs
`tsc --noEmit --traceResolution`, and records what it resolved to. Nothing in
the corpus decides what is correct; the compiler does.

`TypeScript.ResolutionCorpusSpec` then feeds each case through the in-memory
filesystem double and compares. It needs no node, no compiler and no disk, so
the default suite stays fast and hermetic.

Each config is stored as its **literal contents**, written verbatim by the
recorder and read verbatim by the spec, so the two cannot drift about what was
asked.

**A case Deslop does not yet resolve the way tsc does carries a `knownGap` and
is asserted to still differ.** Recording a disagreement rather than deleting
the case is what keeps it visible: closing a gap turns its own test red and
names what changed. Three are recorded today - ESM `.js` specifiers naming a
`.ts` file, a bare `baseUrl`-relative specifier with no `paths` mapping, and an
alias value that already carries its extension.

The corpus is JSON rather than YAML, unlike `bench/reference.yaml`, because it
is written by a zero-dependency Node script and read by aeson. Both do JSON
natively; YAML would need an npm package for no gain.

## Consequences

The claim "Deslop resolves TypeScript the way TypeScript does" is now
evidenced for the cases in the corpus rather than asserted, and widening it is
adding a case and re-recording.

Three real gaps were found by writing it, none of which any existing test
covered.

A corpus diff is a claim about compiler behaviour, so it is reviewed as one:
regenerate deliberately and read the diff, exactly as with `.golden`.

The recorder runs the compiler on generated files, so a corpus case must never
be a file from a real project - it is written by the case, not copied.
