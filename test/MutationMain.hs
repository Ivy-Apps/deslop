{- | The test suite's entry point under the @mutation@ cabal flag: the same hspec
tree, run by sydtest so that sydtest-mutation can drive it.
-}
module Main (main) where

import MutationTree (spec)
import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import Test.Syd (sydTest)
import Test.Syd.Hspec (fromHspec)

main :: IO ()
main = do
    setEnv "GOLDEN_ACTUAL_DIR" =<< getTemporaryDirectory
    sydTest . fromHspec $ spec
