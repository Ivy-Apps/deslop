{- | The test suite's entry point under the @mutation@ cabal flag: the same hspec
tree, run by sydtest so that sydtest-mutation can drive it.
-}
module Main (main) where

import MutationTree (spec)
import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import Test.Hspec (Spec)
import Test.Hspec.Core.Spec (Item (itemRequirement), filterForest, mapSpecForest)
import Test.Syd (sydTest, withTimeout)
import Test.Syd.Hspec (fromHspec)

main :: IO ()
main = do
    setEnv "GOLDEN_ACTUAL_DIR" =<< getTemporaryDirectory
    sydTest . withTimeout instrumentedTimeout . fromHspec . withoutSelfCheck $ spec

{- | This test walks the whole repository through the instrumented library, which
outruns any sane timeout. `just deslop-self` proves the same thing.
-}
withoutSelfCheck :: Spec -> Spec
withoutSelfCheck = mapSpecForest . filterForest $ (/= "checks its own repository clean") . itemRequirement

{- | In microseconds. Under instrumentation the slowest properties outrun
sydtest's default of 60 seconds, and a timeout at baseline would poison every
mutant the test covers.
-}
instrumentedTimeout :: Int
instrumentedTimeout = 300_000_000
