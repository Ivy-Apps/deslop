# 8. Benchmarks are local-only and excluded from CI

Date: 2026-08-16

## Status

Accepted

## Context

`just benchmark` measures `Deslop.doWork` over the fixture projects and fails
when the numbers regress against `bench/reference.yaml`. Every other quality
check in this repository runs in CI, and ADR 0001 states that the justfile is
the source of truth for those checks — `quality.yaml` mirrors `just check` step
for step. A benchmark that CI never runs is therefore a deliberate exception,
and one a future reader is entitled to question.

The exception exists because a wall-clock measurement is only comparable to
another taken on the same machine. Two facts make a CI gate unworkable here:

- **The Reference is machine-specific.** It is recorded by `just
  update-benchmark` on a maintainer's laptop. A GitHub runner is different
  hardware with a different core count; its timings differ from the recorded
  ones by far more than any threshold worth setting, so the gate would fail on
  hardware rather than on code.
- **Shared runners are noisy.** Run-to-run variance from co-tenanted workloads
  is commonly larger than the regression this gate is meant to detect. A
  threshold above that noise floor detects nothing; one below it flaps.

The measurement is also not cheap — the suite takes minutes, against a `just
check` that otherwise runs in seconds.

## Decision

`just benchmark` and `just update-benchmark` are run by hand, on a machine the
maintainer knows. Neither `just check` nor any CI workflow invokes them.

`bench/reference.yaml` records the environment it was taken under — compiler
version, OS, architecture, processor count and capability count — and a run
whose environment differs says so before printing its comparison.

## Considered options

- **Benchmark `main` and the branch in the same CI job, then compare.** The only
  wall-clock design that is statistically sound on shared hardware, because both
  measurements come from the same runner minutes apart. Rejected on cost: it
  doubles the benchmark time and adds a second full GHC build of `main` to every
  pull request, for a signal that a maintainer can obtain locally in three
  minutes.
- **Commit a Reference per machine, keyed by a fingerprint.** Lets CI and local
  runs share one file. Rejected because the CI entry has to be refreshed by a
  human running a workflow, and an entry nobody refreshes rots silently into a
  gate that is either permanently red or meaningless.
- **Gate on allocations only, which are machine-independent.** Would work in CI
  and was seriously considered. Rejected as the *sole* metric, because it cannot
  see a regression that costs time without allocating — a worse constant factor,
  or contention introduced into `pooledMapConcurrentlyN`. Allocations are
  instead gated alongside time, at a tighter threshold, since they are the more
  trustworthy of the two.

## Consequences

- A performance regression can reach `main`. Nothing automated will catch it;
  the maintainer must run `just benchmark` when changing code they believe to be
  hot. This is accepted.
- ADR 0001 no longer holds without qualification: the justfile has a quality
  verb that CI does not mirror. `just check` remains fully mirrored, which is
  the part of that ADR doing the real work.
- `just build` compiles the benchmark (`cabal build --enable-benchmarks all`)
  even though nothing runs it. Without that, the only component nothing in
  `just check` touches would break silently the first time an API it uses
  changes, and stay broken until someone reached for it.
- The Reference is only meaningful next to the machine that produced it. Moving
  to new hardware, or upgrading GHC, invalidates it and requires a fresh `just
  update-benchmark`; the recorded environment is what makes that visible rather
  than mysterious.
- Because the benchmark discards Deslop's output, a green benchmark says nothing
  about correctness — a change that stopped doing half the work would read as a
  large speedup. Benchmark results are only meaningful after a green `just
  check`.
