{- | What a lint pass says about one parsed unit: a Problem, and how to repair it
when it can be repaired.

A pass returns its findings and never acts on them. Whether a repair is applied
depends on the Command and on the Baseline, and both belong to the driver,
"Deslop.Frontend.Pipeline".
-}
module Deslop.Frontend.Finding (
    Finding (..),
    Fix (..),
    applyFindings,
) where

import Deslop.Problem (Problem)
import Deslop.Problem.Baseline (Baseline, inBaseline)

data Finding u = Finding
    { problem :: Problem
    , fix :: Fix u
    }

-- | A repair a Finding offers, as an edit of the unit it was found in.
data Fix u
    = NoFix
    | Rewrite (Endo u)

{- | The unit with every repair applied, except those of Problems the Baseline
has accepted: a baselined Problem is still reported, but its code is left as
the author wrote it.
-}
applyFindings :: Baseline -> u -> [Finding u] -> u
applyFindings baseline unit =
    (`appEndo` unit) . foldMap repair . filter (not . inBaseline baseline . (.problem))
  where
    repair finding = case finding.fix of
        NoFix -> mempty
        Rewrite edit -> edit
