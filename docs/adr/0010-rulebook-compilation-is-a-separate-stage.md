# 10. Rulebook compilation is a separate stage

Date: 2026-08-17

## Status

Accepted

## Context

`Deslop.Rulebook` held four concerns in one module: the YAML wire types, the
domain types, the DTO-to-domain compilation, and the filesystem walk that read
rulebooks off disk. Three things followed from that, and all three were
becoming costly.

The domain type that `Deslop.RuleEnforcer` reads on its hot path imported
`Effectful` and `Effects.FileSystem`, so the filesystem effect came along with
every module that so much as mentioned a `Rule`. Nothing enforced that a
`Rulebook` in hand had been validated, beyond the fact that the only
constructor happened to validate.

Compilation stopped at the first error. `ruleFromDto` was a `do` block in
`Either`, so a rulebook with five broken patterns took five runs to fix, and
the message named the rule and field but not the *file*, which matters because
Deslop loads every `deslop/rules/*.yaml` and more than one can be broken at
once.

[ADR 9](0009-glob-plus-matches-path-segments.md) then added four new
compile-time rules to Glob+, and the Deslop Rules DSL is expected to keep
growing. Compilation is the part that grows with it: more checks, and better
messages for the checks already there.

## Decision

The pipeline gets one module per stage, and each stage can only see the one
before it:

```
bytes ──Loader──▶ RulebookDto ──Compiler──▶ Rulebook ──▶ RuleEnforcer
       (IO)       (may be invalid)  (pure)   (valid, compiled)
```

| Module | Holds |
|---|---|
| `Deslop.Rulebook` | domain types only: pure, no IO, no Aeson |
| `Deslop.Rulebook.Dto` | the YAML shape, and `FromJSON` |
| `Deslop.Rulebook.Compiler` | `RulebookDto -> Either (NonEmpty CompileError) Rulebook` |
| `Deslop.Rulebook.Loader` | the filesystem walk, and the rendered report |

**Errors accumulate**, through a twelve-line `Validation` in `Utils`: an
`Either` with an `Applicative` that combines failures instead of short-circuiting
on the first. Rulebooks, rules and a rule's clauses all accumulate.

**Accumulation stops in exactly one place**, and the asymmetry is deliberate. A
clause compiles against the variables its rule's target binds, so a rule whose
target failed has no scope to check its clauses against. Such a rule reports its
target and its `exclude` patterns, which bind nothing and so need no scope,
and stays silent about its clauses, rather than blaming each of them for a
variable the target never got to define.

**The report is grouped file, then rule, then field**, in source order at every
level, with a count on the first line:

```
Could not load 2 rulebooks.

deslop/rules/broken.yaml
  rule 'provider-components-are-isolated'
    target: "@/components/{{provider}}/**"
      {{provider}} is ambiguous: a single-word name reads as both camelCase and kebab-case.
        ...
  rule 'provider-view-is-unanchored'
    target: "@/**/{{provider-name}}/**/{{FileName}}View"
      {{provider-name}} has ** on both sides, ...
```

Source order, never sorted: the author reads their file from the top, and the
report should match. The count comes first because "how bad is this" is the
first question anyone asks.

`RulebookError` in `Loader` deliberately carries no file path. Only the loader
knows the path, so keeping it out of the error means one rulebook can be
compiled and its failure inspected without an absolute path reaching the answer,
which is what lets `rulebook-from-file--*` be golden-tested at all.

## Considered options

- **Leave it as one module and only add `Validation`.** The smallest change that
  fixes the reported problem. Rejected because it leaves the hot-path domain
  type dragging the filesystem effect behind it, and because the compiler is the
  part that will keep growing, and giving it a module now is cheaper than moving it
  later.
- **Three modules, folding the DTOs into the compiler.** The compiler is the
  only consumer of the DTOs, so they could live with it. Rejected narrowly: the
  DTOs are the *published* file format, and a reader looking for "what may a
  rulebook contain" should not have to read a compiler to find out.
- **Depend on `validation-selective`.** Off-the-shelf and well tested. Rejected
  because the type is twelve lines and its instances are standard; the
  dependency buys `Selective`, which nothing here wants, at the cost of a cabal
  entry, a freeze-file pin and a nix closure.
- **Make `Validation` a `Monad` too.** Would let the target-then-clauses
  dependency be written as a `do` block. Rejected: accumulating and sequencing
  are incompatible, and a lawless `Monad` instance that silently stops
  accumulating is worse than a `case` expression that says so.
- **Report clause errors even when the target failed**, treating every clause
  variable as unbound. More errors per run. Rejected as noise: they would all be
  `UnboundVariable`, all consequences of the one error above them, and they
  would bury it.
- **A flat, ungrouped error list.** Less machinery. Rejected because Deslop
  loads a directory of rulebooks and an error that does not name its file sends
  the author hunting.

## Consequences

- `Deslop.Rulebook` no longer imports `Effectful`, `Effects.FileSystem`,
  `Data.Yaml` or `Data.Aeson` beyond the one `FromJSON` deriving on `RuleId`.
- A run reports every broken pattern at once. `ts-invalid-rulebook-project` now
  carries five errors of five kinds across two rulebook files, and its golden
  pins both the grouping and the order.
- `UI.humanReadable` no longer prefixes rulebook failures with
  `"Could not load Rulebook: "`. The loader's own report already names the count
  and every file, so the prefix said it twice.
- `loadRuleBook`, `ruleBookFromDto`, `ruleBookFromFile` and `parseRuleBookYaml`
  are renamed to their `Rulebook`-spelled equivalents and moved. Nothing outside
  this repository depends on them.
- The failure of one rulebook still aborts the whole run. Enforcing the
  rulebooks that happened to compile would report problems their author never
  asked for and miss the ones they did.
