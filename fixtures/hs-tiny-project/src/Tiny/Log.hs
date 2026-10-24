module Tiny.Log (debug) where

import System.IO (hPutStrLn, stderr)

debug :: String -> IO ()
debug = hPutStrLn stderr
