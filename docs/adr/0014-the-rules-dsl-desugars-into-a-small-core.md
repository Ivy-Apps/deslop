# 14. The Rules DSL desugars into a small core

Date: 2026-08-26

## Status

Accepted

## Context

[#218](https://github.com/Ivy-Apps/deslop/issues/218) asked for two
conveniences: `allows-only:`, which is a `forbids: "**"` and an `allows:` said
in one breath, and `..*`, "zero or many directories back".

Neither needs anything the enforcer cannot already do. Both would nonetheless
land in the middle of the pipeline if added directly, and the pipeline is the
part that has to keep being correct: `Deslop.GlobPlus` decides what a pattern
means, `Deslop.Rule.Book.Compiler` decides what a rulebook may say, and
`Deslop.Rule.Enforcer` runs the result over every module of every project.

The DSL is expected to keep growing - [ADR 10](0010-rulebook-compilation-is-a-separate-stage.md)
already noted that compilation is the part that grows with it. Growing the
surface and growing the core at the same rate is what makes a language expensive.

## Decision

**A rulebook is written in a large language and compiled from a small one.**

```
bytes ──Dto──▶ RulebookDto RuleDto ──Desugar──▶ RulebookDto DesugaredRuleDto ──Compiler──▶ Rulebook
       (IO)     (the surface)                   (the core)                    (valid, compiled)
```

`Deslop.Rule.Book.Desugar` holds the core rule type and the pass that produces
it. `Deslop.Rule.Book.Dto` keeps holding the published file format and nothing
else, so ADR 10's reader test still passes: someone asking "what may a rulebook
contain" reads one module and finds no compiler and no core in it.

**A feature belongs in the desugarer only if all four of these hold:**

1. **Structural** - it rewrites which clauses a rule has, never the inside of a
   glob. Globs are text at that stage; parsing them is the compiler's job.
2. **Expressible** - what it produces uses only what the core already has.
3. **Bounded** - the size of the expansion follows from what was written, not
   from a constant chosen to be big enough.
4. **Compositional** - it survives the combinator its clauses sit under.
   Repeated `forbids` and `allows` are a disjunction, but repeated `uses` and
   `exists` are a conjunction, so a rewrite that turns one clause into many
   means something quite different in each pair.

Anything failing one of them is a change to the *core*, not sugar over it.
`allows-only` passes all four. `..*` fails 1, 3 and 4, and is a Glob+ primitive
instead - see [ADR 15](0015-glob-plus-parent-dir-star-is-a-core-primitive.md).

**`desugarRulebook` is total.** No `Either`, no scope, no IO. That is the
invariant, not an accident of how little sugar there is today: the day a sugar
feature needs to fail is the day it stopped being sugar, and it should be
implemented somewhere that can say so properly. Property D2 pins it.

**`allows-only: [x]` always means `forbids: "**"` plus `allows: [x]`**, appended
to whatever the rule already said rather than replacing it. The combination is
meaningful: the generated forbid covers direct imports only, so a hand-written
transitive one still adds something, and a rule's `allows` carve out of all of
its `forbids` either way.

**The rulebook envelope is parameterised over its rule**, `RulebookDto rule`,
because only a *rule* has sugar in it. Desugaring a file is then `fmap
desugarRule`, and the phase is legible in every signature.

### How this compares to GHC

The design is GHC's, with one deliberate difference. GHC's own rule is the same
one: a local, meaning-preserving, size-bounded translation into existing Core is
desugared; a genuinely new semantic notion extends Core. Core is not System F,
it is System F_C - System F **plus coercions** - because GADTs and type families
could not be desugared away soundly, so Core grew a `Cast` constructor instead.
`..*` is that case.

The difference is *where in the pipeline this pass sits*. GHC desugars fourth,
after renaming and typechecking, so no error message ever names a construct the
user did not write. This pass runs before compilation, which is GHC's `deriving`
position rather than its desugarer position: `deriving` generates surface-level
`HsSyn` and pushes it back through the normal pipeline, precisely because what
it generates is code the author could have written by hand. `allows-only`
qualifies - its output is an ordinary rulebook.

## Considered options

- **Add both features to the compiler directly.** The smallest change. Rejected
  because it grows the part of the system that has to keep being correct at the
  same rate as the part that only has to be convenient, which is what makes a
  language expensive to keep.
- **Desugar `..*` too**, into `MAX_DEPTH` repetitions of `..`. What the issue
  proposed. Rejected under conditions 1, 3 and 4 - see ADR 15 for the detail.
- **Trees That Grow**: one `RuleDto p` indexed by phase, with a type family
  making sugar fields unrepresentable in the core. What GHC actually uses, and
  genuinely future-proof. Rejected for now: for one differing field it costs
  type families, standalone deriving with constraints and a hand-written
  `FromJSON`, and GHC itself only reached for it after three hand-maintained AST
  copies became untenable. Two records and a type parameter are the honest size
  of the problem today.
- **Two full records**, `RulebookDto` and `DesugaredRulebookDto`. Rejected
  narrowly: the envelope is identical either side of the pass, so parameterising
  it removes a whole record and turns the traversal into the `Functor` that was
  already there.
- **Put `DesugaredRuleDto` in `Dto`** and leave `Desugar` a pass-only module.
  A cleaner dependency graph - the compiler would import one module instead of
  two. Rejected because `Dto` would then hold a type that is not part of the
  file format, which is exactly the reader test ADR 10 set.
- **Model `forbids`/`allows`/`allows-only` as a sum type** so the combination is
  unrepresentable. Rejected: the combination is not an impossible state, the
  transitive case above is a real use, and it would cost a hand-written
  `FromJSON` whose failure mode is a raw aeson message.
- **Carry provenance through the pass**, so a compile error in an `allows-only`
  glob is labelled `allows-only.import` rather than `allows.import`. Rejected as
  not worth its cost here - see Consequences.

## Consequences

- `RulebookDto` gains a type parameter. `parseRulebookYaml` returns
  `RulebookDto RuleDto`; `compileRulebook` takes `RulebookDto DesugaredRuleDto`.
  `Deslop.Rule.Book.Loader` is the one place they meet, in one line.
- **A rulebook key of two or more words is kebab-case.** `allowsOnly` is written
  `allows-only`. This is the file format's first multi-word key, so the DTOs
  swap `deriving anyclass (FromJSON)` for `genericParseJSON` with
  `fieldLabelModifier = camelTo2 '-'`. Every existing key is a single word and
  is unaffected, but the spelling is now settled for every key added after this
  one.
- **A compile error in an `allows-only` glob is labelled `allows.import`.** The
  glob itself is quoted verbatim, so the author can still find it, but the key
  named is not the one they wrote. Accepted deliberately: preserving provenance
  means threading the source field through the core language, and it buys one
  word in one message. If a second sugar feature ever feeds an existing field,
  this should be revisited - the cost grows with each one.
- `fixtures/rulebook/syntax-sugar.yaml` and its
  `rulebook-from-file--syntax-sugar` golden are this repository's `-ddump-ds`:
  the golden *is* the desugared, compiled output, so what any piece of sugar
  expands to is readable in a diff.
- Properties D1-D3 join the suite. D1 is differential against the longhand a
  user would have written, which is the whole claim `allows-only` makes. All
  three generate arbitrary glob *text*, which tests condition 1 rather than
  asserting it.
- Sugar that would pass all four conditions, if it is ever wanted: `target: [a,
  b]` expanding to several rules, a bare-string shorthand for the object clause
  forms, rule inheritance, presets, and clause defaults. None needs a constant
  and none touches glob internals.
