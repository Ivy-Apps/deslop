# 3. Import cycles are reported per strongly connected component

Date: 2026-08-06

## Status

Accepted

## Context

`no-import-cycles` reports circular imports as `LintProblem`s, which means every
cycle it finds becomes a baseline key of the form
`{rule-id}#{relative-file-path}`. Those keys are written to `deslop/baseline.yaml`
and live in the user's repository indefinitely, so whatever the check reports has
to be both bounded in size and stable across runs.

A module graph does not have one obvious notion of "a cycle". Three units are
available, and they differ enormously:

- **Elementary cycles.** Every distinct loop, enumerable with Johnson's
  algorithm. Their number is exponential in the worst case: a densely connected
  component of twenty modules produces millions of them.
- **Back edges.** Every edge that closes a loop during a depth-first traversal.
  Bounded by the edge count and pleasantly actionable, since each one names a
  single import to delete. But which edges are back edges depends on where the
  traversal starts, and one module can close several of them.
- **Strongly connected components.** The maximal groups of mutually reachable
  modules. `Data.Graph` computes them directly, they partition the graph, and
  each one corresponds to exactly one knot of mutual dependency.

## Decision

One problem per strongly connected component that is cyclic — more than one
module, or a single module importing itself.

The component is reported against its alphabetically first module, and the
description shows the shortest loop from that module back to itself, with
neighbours visited in module-id order so ties resolve deterministically.

## Considered options

- **Enumerate every elementary cycle with Johnson's algorithm.** Matches the
  plain reading of "find all cycles", and is what a user might expect. Rejected
  on output size: a single tangled component could add millions of lines to a
  check report and millions of keys to a baseline file, and no one is going to
  act on the ten-thousandth variation of the same knot.
- **Report every back edge.** Bounded output, and each problem points at one
  concrete import statement. Rejected because the set of back edges depends on
  traversal order, and one file can close several cycles. Both properties break
  the baseline: `no-import-cycles#src/a.ts` would have to stand for several
  distinct problems, and which ones it stood for could change between runs.

Per-component reporting avoids both. A module belongs to exactly one strongly
connected component, so the file-keyed baseline format already fits it exactly —
two different cycles can never collide on one key.

## Consequences

- A component holding several interwoven loops reports one witness loop, not all
  of them. Breaking the printed loop may reveal another one in the same
  component on the next run. This is the honest shape of the problem: a tangle of
  ten mutually dependent modules is one architectural defect, not forty-two.
- Baseline keys are stable against file traversal order and against the
  concurrent parse scheduling in `deslopProject`. They are *not* stable against
  the component's membership changing: if a new module joins a component and
  sorts before the current start, the key changes and the cycle resurfaces for
  review. That is intended — the cycle genuinely changed shape.
- Finding cycles costs one `Data.Graph.scc` pass over the graph that
  `buildModuleGraph` already produces, plus one bounded breadth-first search per
  cyclic component.
