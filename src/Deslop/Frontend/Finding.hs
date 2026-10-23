{- | What a lint pass says about one parsed unit: a Problem, and how to repair it
when it can be repaired.

A pass returns its findings and never acts on them. Whether a repair is applied
depends on the Command and on the Baseline, and both belong to the driver,
"Deslop.Frontend.Pipeline".

The repair is the single source of truth for whether a Finding is auto-fixable.
The constructor is not exported: a pass hands 'fixable' or 'unfixable' its
Problem as a function of that flag, and they supply it from the repair, so a
Finding that claims a fix it does not carry, or carries one it does not claim,
cannot be built.
-}
module Deslop.Frontend.Finding (
    Finding,
    problem,
    applyFindings,
    fixable,
    unfixable,
    within,
) where

import Deslop.Problem (Problem)
import Deslop.Problem.Baseline (Baseline, inBaseline)

data Finding u = Finding !Problem !(Fix u)

-- | What a Finding reports.
problem :: Finding u -> Problem
problem (Finding p _) = p

-- | A repair a Finding offers, as an edit of the unit it was found in.
data Fix u
    = NoFix
    | Rewrite !(Endo u)

{- | The unit with every repair applied, except those of Problems the Baseline
has accepted: a baselined Problem is still reported, but its code is left as
the author wrote it.
-}
applyFindings :: Baseline -> u -> [Finding u] -> u
applyFindings baseline unit =
    (`appEndo` unit) . foldMap repair . filter (not . inBaseline baseline . problem)
  where
    repair = \case
        Finding _ NoFix -> mempty
        Finding _ (Rewrite edit) -> edit

-- | A Problem that @deslop fix@ repairs with this edit.
fixable :: (Bool -> Problem) -> Endo u -> Finding u
fixable problemOf = Finding (problemOf True) . Rewrite

-- | A Problem nothing can repair unattended.
unfixable :: (Bool -> Problem) -> Finding u
unfixable problemOf = Finding (problemOf False) NoFix

{- | A Finding about one part of a unit, as a Finding about the whole: its repair
edits that part, through @inside@, and leaves the rest as it was.
-}
within :: (Endo part -> Endo whole) -> Finding part -> Finding whole
within inside (Finding p offered) = Finding p (lifted offered)
  where
    lifted = \case
        NoFix -> NoFix
        Rewrite edit -> Rewrite (inside edit)
