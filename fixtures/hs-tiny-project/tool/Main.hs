module Main (main) where

import Cli (banner)
import Tiny.A (a)

main :: IO ()
main = print (banner, a)
