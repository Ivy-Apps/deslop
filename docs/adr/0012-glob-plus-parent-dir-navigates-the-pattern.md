# 12. Glob+ `..` navigates the pattern, not the path

Date: 2026-08-18

## Status

Accepted

## Context

A clause could name a directory only by writing it out, or by starting from
`{{TARGET_DIR}}` and going *down*. There was no way to go up. The shape people
wanted ([#197](https://github.com/Ivy-Apps/deslop/issues/197)) is a rule
that permits a feature to reach its sibling `shared/` folder and nothing else:

```yaml
target: "@/client/{{feature-name}}/{{FileName}}View"
forbids:
  - import: "@/client/**"
allows:
  - import: "{{TARGET_DIR}}/**"
  - import: "{{TARGET_DIR}}/../shared/**"
```

Written out longhand, the second `allows` would have to name every feature's
sibling separately, which is exactly the repetition a captured variable exists
to remove.

`..` is the universal spelling of "one directory back", so the token was never
in question. What it *operates on* was.

## Decision

**`..` cancels the segment to its left in the hydrated pattern.** It is
resolved by `resolveSteps` before the structural match begins; the matcher is
unchanged and never sees one.

The alternative reading - that `..` moves a cursor backwards over the candidate
path while matching - is not merely worse, it does not work. Under it the clause
above rejects the import it exists to allow:

```
clause hydrates to   [@] [client] [home] [..] [shared] [**]
candidate            @/client/shared/Button

the matcher consumes @ and client, then must consume "home" against the
candidate's third segment, which is "shared" - dead before the [..] is reached.
```

The pattern side is the only side with anything to navigate. `{{TARGET_DIR}}` is
concrete text by hydration time, exactly like a real directory, and it is the
thing the author is navigating *from*. The candidate path is the thing being
*tested*; it has no cursor to walk backwards. So cancelling on the pattern is
not a weakening of Unix semantics - it is the only place Unix semantics can be
applied.

Three rules follow, all decided at compile time:

**`..` belongs to clause patterns alone.** A target or an exclude is matched
against whole module ids and has no directory to be relative to. It is
unrepresentable rather than rejected: `CompiledClausePattern` holds
`[Step (PatternSegment ClauseVar)]` where `data Step a = ParentDir | Step a`,
and the other two patterns are not built from `Step` at all.

**`..` may only go back past a segment whose text the pattern determines** -
a literal, a variable, or `{{TARGET_DIR}}`. A `**` is variable-width, so "one
directory back" from it names nothing in particular; a segment holding a `*`
determines no directory either, and cancelling it would make the `*` the author
wrote a silent no-op.

`checkParentDirs` decides this by simulating the cancellation over the compiled
steps: each step contributes one token, each `..` takes one back. That is never
more of the pattern than hydration has, because a `{{TARGET_DIR}}` step becomes
several segments there and one token here - so the check reaches an earlier step
sooner than hydration can, and never later. Hydration is therefore guaranteed
never to meet a `**` or a wildcard, which is what lets it be a plain `drop 1`
that cannot fail.

**A `..` with nothing left to go back past does nothing**, as `/..` is `/` on
Unix. This is what makes the check above sufficient without knowing how deep
`{{TARGET_DIR}}` will expand.

## Considered options

- **`..` as a cursor over the candidate path.** Rejected: it makes the
  motivating clause match nothing at all, as shown above. There is no candidate
  it accepts that the pattern reading does not.
- **Reject a `..` that can go back past the start.** The target pattern bounds
  `{{TARGET_DIR}}`'s depth from below - `matchTarget` refuses any path shorter
  than `minLength`, and `{{TARGET_DIR}}` is that path minus its last segment -
  so `{{TARGET_DIR}}/../../../x` under a three-segment target could have failed
  to compile. Rejected in favour of the Unix clamp: it costs threading the
  target's `minLength` into the clause compiler, and it makes `..` behave unlike
  `..` for the sake of one authoring mistake. The cost is recorded under
  Limitations in the README - a clause that clamps past the root matches
  nothing, silently.
- **Cancel `*` and `**` too.** One rule, no new errors. Rejected because
  `@/client/**/../shared/**` has two equally defensible readings - cancel the
  whole `**`, or let `**` absorb the `..` - and where two readings are
  defensible the author should be made to choose. Silently discarding a `**`
  would also narrow a rule without saying so, which is the false-negative
  direction the polarity rule in
  [ADR 9](0009-glob-plus-matches-path-segments.md) exists to avoid.
- **Add `ParentDir` to `Seg`.** The smallest diff. Rejected because `Seg`
  answers "how many path segments does this consume" - `GlobStar` many,
  `Segment` one - and `..` consumes none; it edits the list before consumption
  starts. Putting it there would also make it representable in
  `CompiledTargetPattern`, so `minSegments`, `walkSegments` and `matchExclude`
  would each need an arm for a state the compiler promises cannot occur, with no
  `absurd` to prove it.
- **Parameterise `Seg` over the capability**, as `SegPart Void` already does for
  variables in excludes. Type-safe and consistent. Rejected because the
  parameter would thread through every signature mentioning `Seg` across the
  matcher, none of which has anything to do with `..`.
- **Make `.` a no-op too**, completing the Unix normalisation. Rejected: `.` is
  already load-bearing as literal text, since `directoryOf` returns `"."` for a
  path with no directory, so a hydrated `{{TARGET_DIR}}` can *be* the segment
  `.`. Making it structural would change what existing rules mean.
- **Treat `..` glued to text as an error**, mirroring `GlobStarNotWholeSegment`.
  Rejected: unlike `**`, a dotted name has an obvious ordinary reading, and
  module ids in this repository's own fixtures carry `.spec` and `.stories`.
  Only the exact token `..` is structural.

## Consequences

- `CompiledClausePattern.segments` becomes `.steps`, with a new type. The
  `rulebook-from-file--*` golden records the shape and was regenerated.
- `renderClausePattern` and `moduleFromGlob` resolve `..` before rendering, so a
  `uses:` violation says `must import '@/client/shared/registry'` rather than a
  path the author would have to resolve by hand. Both share `resolveSteps` with
  `hydrate`, so `..` is implemented once.
- `{{TARGET_DIR}}` is the directory of the matched *file*, so under a target
  containing `**` the same `..` clause resolves differently for files at
  different depths. This is documented in `docs/GLOB+.md` and pinned by the
  `nested-view-uses-its-shared-formatter` rule in `ts-globplus-project`.
- Properties P23-P26 join the suite; `GlobPlusOracle` gains `resolveParentDirs`
  and `parentDirsLegal` as the references they measure against.
