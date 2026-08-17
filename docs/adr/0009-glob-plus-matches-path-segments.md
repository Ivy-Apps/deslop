# 9. Glob+ matches path segments, not characters

Date: 2026-08-17

## Status

Accepted. Refines [ADR 6](0006-glob-plus-variables-are-named-with-inferred-casing.md)
and [ADR 7](0007-glob-plus-values-agree-by-spelling-compatibility.md); the
Decision below says clause by clause what it keeps from each and what it
replaces.

## Context

Glob+ compiled a pattern into a POSIX ERE and handed matching to `regex-tdfa`.
The engine's match policy was therefore the language's semantics, and POSIX
longest-match is the wrong policy for a pattern that captures.

The failure was measured against `d34bf41` rather than reasoned about, as a
matrix of pattern × path × captured value. Three groups of it are wrong:

**A variable between two `**` binds the wrong segment.**

| Pattern | Path | Captured |
|---|---|---|
| `@/**/{{provider-name}}/**/{{FileName}}View` | `@/stripe-connect/CheckoutView` | `provider-name=stripe-connect` ✅ |
| `@/**/{{provider-name}}/**/{{FileName}}View` | `@/stripe-connect/payment/CheckoutView` | **`provider-name=payment`** ❌ |
| `@/**/{{provider-name}}/**/{{ProviderName}}Entry` | `@/stripe-connect/a/StripeConnectEntry` | **NO MATCH** ❌ |

The same pattern is right at depth 1 and wrong from depth 2 on. Several splits
satisfy it, POSIX gives the leftmost subexpression the longest text it can take,
and that is not the split the author meant. Where the variable is repeated the
symptom changes from a wrong value to no match at all, because agreement was
checked *after* the split was chosen and never got to reject it.

**Agreement validated a split instead of choosing one.**

```
@/c/{{provider-name}}/{{provider-name}}-{{service-type}}
  vs @/c/stripe/stripe-connect-payment          →  NO MATCH
```

`stripe` / `connect-payment` satisfies the pattern, but the greedy division of
the last segment was the only one tried.

**A `*` between two variables is not a boundary.** `checkAdjacency` rejected
`{{A}}{{B}}` for having no boundary, while `{{A}}*{{B}}` compiled and had
exactly the same problem, since `*` can match empty.

Two more things were wrong in the same direction. `a/**` did not match `a`, so a
`forbids: "@/internal/{{FileName}}/**"` missed an import of `@/internal/Foo`
sitting beside the folder. And `matchClause` built a regex on every call - once
per import, per clause, per matched rule - which the module's own `TODO(perf)`
admitted.

The common thread is that the *path* decided what a pattern meant. For a linter
that is the worst failure available: a rule covering less than its author
believes reports nothing, and nothing is what you see.

## Decision

**Glob+ matches a list of path segments.** A pattern is a list of segment
patterns; `**` is the only token that varies how many segments are consumed, and
it must occupy a whole segment. Everything else consumes one segment or part of
one.

```haskell
data Seg a     = GlobStar | Segment a
data SegPart v = Lit Text | AnyChars | VarPart v
```

Six rules follow, and they are chosen together because each one exists to make
a pattern's meaning independent of the path it meets.

- **`**` is zero or many segments, everywhere.** `a/**` matches `a`. There is no
  special trailing form, because a token that means one thing in the middle and
  another at the end is the disease rather than the cure.

- **`**` glued to text does not compile.** `@/a/**View` is
  `GlobStarNotWholeSegment`. Use `*View` to match within a segment.

- **A target's variable may not have `**` on both sides.** `UnanchoredVariable`.
  This is the load-bearing one. Every variable's segment index becomes a
  function of the pattern and the path length alone, and therefore:

  > **No choice of globstar widths can change what anything binds.**

  The two searches in the matcher are independent because of it. The outer walk
  picks globstar widths and decides only the boolean, so its order is an
  implementation detail that can be swapped for a faster one without
  re-litigating semantics. The inner walk divides a single segment among its
  parts, and that order is the only observable one left in the engine.

- **Two variables in a segment need a literal between them.**
  `NoBoundaryBetween`, replacing ADR 6's `AdjacentVariables` and now catching
  `{{A}}*{{B}}` as well as `{{A}}{{B}}`. One syntactic sentence, no character
  class reasoning.

- **Agreement is a constraint carried through the match, not a check after it.**
  Each variable holds the names still able to spell every occurrence seen so
  far; a branch whose set empties dies there. Within a segment, divisions are
  enumerated greedy-left and *the first that satisfies every constraint wins* -
  so `@/c/{{p}}/{{p}}-{{s}}` now binds `p=stripe`, `s=connect-payment`.
  Backtracking is global across segments: a later segment may reject an earlier
  one's division.

- **Polarity becomes `Widen | Narrow`**, and the direction is chosen to
  minimise false negatives. A false positive is visible and can be silenced with
  `exclude`, `allows` or the baseline; a rule that quietly stops enforcing
  cannot be seen at all.

  | Field | Polarity | Why |
  |---|---|---|
  | `target` | `Widen` | a target that fails to match is a rule that never fires |
  | `exclude` | n/a | no variables, so nothing to guess |
  | `forbids` | `Widen` | an unrecognised spelling is a violation gone unreported |
  | `allows` | `Narrow` | an exemption is a licence, and licences should be literal |
  | `uses` | `Narrow` | a match *silences* a report; widening can only remove one |
  | `exists` | `Narrow` | as `uses` |

  `uses` and `exists` are the two places this reverses the intuition that
  "widen everywhere is safer". They are the clauses where matching means
  *satisfied*, so `Widen` there is purely the false-negative direction: a target
  capturing `ABTest` proposes both `ab-test` and `a-b-test`, and a widened
  `uses: @/lib/{{file-name}}` would accept a legacy `@/lib/a-b-test` in place of
  the required `@/lib/ab-test` and report nothing.

- **`regex-tdfa` is dropped.** `Deslop.GlobPlus` was its only user.

Two consequences for the hot path fall out rather than being aimed at. A path is
split into segments once per module instead of once per match, and a clause is
hydrated once per matched target instead of once per candidate import - which
retires the `TODO(perf)` by deleting the regex rather than caching it.

### What this keeps and what it replaces

*Kept from ADR 6*: casing inferred from spelling; ambiguous single-word and
consecutive-capital names rejected; `{{TARGET_DIR}}` reserved in every casing;
a clause variable must be bound by its rule's target; three pattern types, with
`ExcludePattern` parameterised by `Void` so a variable in one is unrepresentable.

*Replaced from ADR 6*: the `**/` idiom and its optional-group encoding, and the
capture-group numbering that existed to work around it - both gone with the
regex. `AdjacentVariables` is generalised to `NoBoundaryBetween`. "The leftmost
variable binds greedily under POSIX longest-match" survives as behaviour but is
now our own enumeration order rather than an engine's policy.

*Kept from ADR 7*: agreement asks whether some one name spells every occurrence;
the coarsest reading is what gets written out; patterns are strict and values
lenient.

*Replaced from ADR 7*: `Forbidding`/`Requiring` become `Widen`/`Narrow`, and
`target` gains an explicit polarity it never had. Its capture-group numbering
fix is moot. Most importantly, ADR 7 left "should agreement participate in
choosing the split, or only validate one?" open; this decision answers it -
participate.

## Considered options

- **Keep the regex and add a fallback**: on agreement failure, search other
  splits. Fixes the repeated-variable case at no cost to what already works.
  Rejected because it leaves the *unrepeated* case silently wrong - group B's
  first row binds `payment` and no agreement check will ever notice - and
  because it keeps the engine's match policy as the language's semantics.
- **Keep unanchored variables legal with a defined tie-break**, e.g.
  leftmost-shortest. Deterministic and documentable, and it breaks no existing
  rulebook. Rejected because it does not fix anything: one pattern still means
  "the provider folder" at depth 3 and "whatever sits last" at depth 5. The
  author cannot write what they mean, so a defined wrong answer is still wrong.
  Rejecting the pattern tells them so at load time.
- **Reject a separator both variables could consume**, so
  `{{provider-name}}-{{service-type}}` fails to compile. Would kill the last
  within-segment ambiguity outright. Rejected because deciding it needs an
  emptiness-of-intersection procedure over the casings' character classes -
  `{{provider-name}}-{{ServiceType}}` is in fact unambiguous, since the right
  side must start with a capital - and because it bans a documented, working
  idiom. The constraint search resolves the real cases anyway.
- **A trailing `**` meaning one-or-more**, preserving "`a/**` is strictly below
  `a`". Rejected: position-dependent meaning is the class of bug this ADR exists
  to remove.
- **Widen `uses` and `exists` too**, for one uniform rule. Simpler to state and
  it removes the acronym false positives pinned in `ts-casing-project`.
  Rejected on the stated principle: those are the clauses where widening can
  only ever delete a report.
- **A two-pointer or memoised globstar search**, guaranteeing O(n·m). Rejected
  as premature: patterns carry 0-2 globstars and paths 3-10 segments, so the
  naive search is bounded by ~100 rejects in the worst realistic case. It is
  also the safest thing to defer, precisely because the anchoring rule makes
  swapping it a pure change.
- **Merge with `Git.Ignore`'s glob engine.** ADR 5 kept them apart partly
  because they were structurally different, and after this change both walk
  segments - so the question was reopened. Still rejected: `**` means something
  different in gitignore(5), which also has negation, directory-only rules and a
  first-match-wins verdict, while Glob+ has variables and a `Maybe Bindings`
  result. The shared part would be twenty lines of the same *shape*, not the
  same *concept*. A merge becomes plausible if Glob+ grows toward the full glob
  standard, and that is the direction to revisit it from.

## Consequences

- **Every shipped rulebook still compiles.** All 90 patterns across
  `examples/rules/` and `test/fixtures/*/deslop/rules/` were checked: not one
  puts a variable between two globstars. The anchoring rule costs no shipped
  content, though that is evidence about these files rather than a guarantee
  about rulebooks in general.
- **`a/**` matching `a` does change results**, and visibly:
  `@/components/{{provider-name}}/**` now applies to the module
  `@/components/paypal` sitting beside its folder, which is pinned in
  `ts-globplus-project`.
- **Four shapes that used to compile no longer do**: a variable between two
  globstars, `{{A}}*{{B}}`, `**` glued to text, and - unchanged from ADR 6 -
  a variable in an exclude pattern.
- **The property suite is the specification.** P0-P22 in
  `test/Deslop/GlobPlusPropSpec.hs` are numbered and referenced from here. P0
  compares against a brute-force oracle in `test/Deslop/GlobPlusOracle.hs` that
  enumerates every split; P6 and P7 state the anchoring theorem directly; P8
  states that a satisfying assignment is always found.
- **`Deslop.GlobPlusModel` is gone.** It modelled only whole-segment slots and
  had to plant a `.` in every globstar segment so a greedy `**` could not steal
  a variable's text. The oracle needs no such trick, because the anchoring rule
  makes the intended parse the only one. The model getting simpler is evidence
  about the design, not just about the test.
- **One real bug was found by P0 during the rewrite, and is worth recording.**
  Narrowing a variable's candidate names by intersecting each new occurrence's
  `decodings` is wrong: `A00` proposes `a00` and `a 00`, `A_0_0` proposes
  `a 0 0`, and the name that spells both is proposed by neither first. Agreement
  must be re-asked over all occurrences rather than narrowed - which is what
  `Deslop.Casing.agree` already did, so it is called per occurrence instead of
  being reimplemented.
- **Compiled patterns are now `Show`able and `Eq`.** The
  `rulebook-from-file--page-architecture` golden reads as structure instead of
  `<regex>`, which is a better specification of what compilation produced.
