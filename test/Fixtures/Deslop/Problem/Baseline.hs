-- | Baselines built straight from 'Deslop.Problem.ProblemId' text.
module Fixtures.Deslop.Problem.Baseline (baselineOf) where

import Data.HashSet qualified as HS
import Deslop.Problem (ProblemId (..))
import Deslop.Problem.Baseline (Baseline (..))

baselineOf :: [Text] -> Baseline
baselineOf = Baseline . HS.fromList . fmap ProblemId
