module Main (main) where

import Deslop (runDeslop)
import Params (optsInfo, parserPrefs)
import Options.Applicative (customExecParser)

main :: IO ()
main = do
    params <- customExecParser parserPrefs optsInfo
    runDeslop params
