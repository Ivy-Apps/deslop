{- | The test suite's entry point under the @mutation@ cabal flag: the same hspec
tree, run by sydtest so that sydtest-mutation can drive it.
-}
module Main (main) where

import Data.List (isInfixOf)
import E2E.ProjectGoldenSpec (wholeRepositoryRuns)
import MutationTree (spec)
import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import Test.Hspec (Spec)
import Test.Hspec.Core.Spec (Item (itemRequirement), filterForest, mapSpecForest)
import Test.Syd (sydTest, withTimeout)
import Test.Syd.Hspec (fromHspec)
import TestUtils (goldenActualDirEnv)

main :: IO ()
main = do
    setEnv goldenActualDirEnv =<< getTemporaryDirectory
    sydTest . withTimeout instrumentedTimeout . fromHspec . withoutWholeRepositoryRuns $ spec

{- | Under instrumentation a run over a whole real application takes minutes, and
every surviving mutant pays for every test that covers it. So a mutant only a
real application kills survives here. The pull request, source base and changed
tags of explore stay covered by @explores ts-cycles-project@, and
`just deslop-self` runs the check of this repository in CI.
-}
withoutWholeRepositoryRuns :: Spec -> Spec
withoutWholeRepositoryRuns = mapSpecForest . filterForest $ not . isWholeRepositoryRun . itemRequirement
  where
    isWholeRepositoryRun requirement = any (`isInfixOf` requirement) wholeRepositoryRuns

{- | In microseconds. Under instrumentation the slowest properties outrun
sydtest's default of 60 seconds, and a timeout at baseline would poison every
mutant the test covers.
-}
instrumentedTimeout :: Int
instrumentedTimeout = 300_000_000
