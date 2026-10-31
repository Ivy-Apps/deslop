{- | The test suite's entry point under the @mutation@ cabal flag: the same hspec
tree, run by sydtest so that sydtest-mutation can drive it.
-}
module Main (main) where

import Data.List (isInfixOf)
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
every surviving mutant pays for every test that covers it. These walk the same
code as the small fixture projects, and `just deslop-self` proves the last one.
-}
withoutWholeRepositoryRuns :: Spec -> Spec
withoutWholeRepositoryRuns = mapSpecForest . filterForest $ not . isWholeRepositoryRun . itemRequirement
  where
    isWholeRepositoryRun requirement =
        any (`isInfixOf` requirement) ["melzar-nextjs-clean-architecture", "ixartz-next-js-boilerplate", "checks its own repository clean"]

{- | In microseconds. Under instrumentation the slowest properties outrun
sydtest's default of 60 seconds, and a timeout at baseline would poison every
mutant the test covers.
-}
instrumentedTimeout :: Int
instrumentedTimeout = 300_000_000
