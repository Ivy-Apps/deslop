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
    fixable,
    unfixable,
    applyFindings,
) where

import Deslop.Problem (Problem)
import Deslop.Problem.Baseline (Baseline, inBaseline)
import GHC.Records (HasField (..))

data Finding u = Finding Problem (Fix u)

-- | A repair a Finding offers, as an edit of the unit it was found in.
data Fix u
    = NoFix
    | Rewrite (Endo u)

-- | The Problem a Finding reports, auto-fixable exactly when it carries a repair.
instance HasField "problem" (Finding u) Problem where
    getField (Finding problem _) = problem

-- | A Problem that @deslop fix@ repairs with this edit.
fixable :: (Bool -> Problem) -> Endo u -> Finding u
fixable problem = Finding (problem True) . Rewrite

-- | A Problem nothing can repair unattended.
unfixable :: (Bool -> Problem) -> Finding u
unfixable problem = Finding (problem False) NoFix

{- | The unit with every repair applied, except those of Problems the Baseline
has accepted: a baselined Problem is still reported, but its code is left as
the author wrote it.
-}
applyFindings :: Baseline -> u -> [Finding u] -> u
applyFindings baseline unit =
    (`appEndo` unit) . foldMap repair . filter (not . inBaseline baseline . (.problem))
  where
    repair = \case
        Finding _ NoFix -> mempty
        Finding _ (Rewrite edit) -> edit

