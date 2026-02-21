module Main (main) where

import Deslop (runDeslop)
import Options.Applicative (customExecParser)
import Params (paramsParser, parserPrefs)

main :: IO ()
main =
    customExecParser parserPrefs paramsParser
        >>= runDeslop
