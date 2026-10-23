# Deslop's rules for itself

`rules/layering.yaml` states the source tree's layering
([ADR 13](../docs/adr/0013-the-source-tree-is-layered-by-dependency-direction.md),
[ADR 20](../docs/adr/0020-languages-plug-in-through-a-staged-compiler-frontend.md))
as Rulebook Rules over Haskell Module Names.

It is inert until Deslop has a Haskell frontend: running Deslop on this
repository loads it, then stops, finding no supported project. It must still
load, because a Rulebook that does not is reported before anything else.
Until then the layering is checked by hand, with
`grep -rn "^import Frontend" src/Deslop/`.

Keep this file out of `rules/`: Deslop reads every file there as a Rulebook.
