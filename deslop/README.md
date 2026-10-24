# Deslop's rules for itself

`rules/layering.yaml` states the source tree's layering
([ADR 13](../docs/adr/0013-the-source-tree-is-layered-by-dependency-direction.md),
[ADR 20](../docs/adr/0020-languages-plug-in-through-a-staged-compiler-frontend.md))
as Rulebook Rules over Haskell Module Names, and Deslop checks itself with
them through its own Haskell frontend
([ADR 21](../docs/adr/0021-haskell-is-read-as-one-cabal-package.md)):

```bash
nix develop .#ci -c just deslop-self
```

It runs as part of `just check` and in CI, so a violation fails the build.

## How the rules name modules

Every module is canonically named by its component - `lib/deslop/Deslop/Module`,
`test/deslop-test/Frontend/TypeScriptSpec`, and for a `main-is`
`exe/deslop/Main`, `test/deslop-test/Main` or `bench/deslop-bench/Main` - so
canonical names never clash, and that is what reports and `baseline.yaml`
show. The bare name, `Deslop/Module`, is an alias, and a Rule matches either.
Rules about the source tree target `lib/*/...`, so a spec named after a
language is never mistaken for part of it. Modules of other
packages are named the same way: `System/IO`, `Data/Text`.

## The rules

| Rule | Says |
|------|------|
| `core-is-language-agnostic` | `Deslop/**` reaches no language, nor `Params` or `UI` |
| `languages-are-isolated` | a language imports no other language, the registry or the orchestration |
| `only-the-registry-imports-languages` | outside tests, only `Frontend` imports `Frontend/<Lang>/**` |
| `effects-are-domain-free` | `Effects/**` names nothing of the domain but `Deslop/Problem` |
| `only-effects-cli-touches-the-terminal` | no library module but `Effects/CLI` imports `System/IO` and friends |
| `ui-is-pure` | `UI` imports no effect |
| `infrastructure-is-domain-free` | `FileSystem/**`, `Git/**`, `Utils`, `Renderable` import neither the core nor a language |
| `library-modules-have-specs`, `top-level-library-modules-have-specs` | every library module has `test/deslop-test/<Path>Spec` |
| `test-utils-are-domain-free` | `TestUtils` imports neither the core nor a language |
| `test-helpers-stay-in-tests` | only test modules import `Fixtures/**`, `Generators/**`, `Doubles/**` |

## The baseline

`baseline.yaml` accepts the library modules that had no spec when the rules
were written. Write the spec and the entry stops matching; regenerate with
`cabal run deslop -- baseline .` only to accept a gap deliberately. A
layering violation is fixed, or the Rule is - it is never baselined.

Keep this file out of `rules/`: Deslop reads every file there as a Rulebook.

## The diagram

[`diagram/index.html`](diagram/index.html) is Deslop's own module graph,
written by `deslop diagram .`. Download it and open it in a browser: it is one
self-contained page that works offline. It is a snapshot, not checked by CI;
regenerate it with:

```bash
"$(nix develop .#ci -c cabal list-bin deslop)" diagram .
```
