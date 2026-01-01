module Main (main) where

import Deslop (runDeslop)
import System.Environment (getArgs)

main :: IO ()
main = do
    [projectPath] <- getArgs
    runDeslop projectPath
