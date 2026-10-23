module Main (main) where

import Cli (banner)
import Tiny.A (a)
import "hs-tiny" Tiny.D (d)

main :: IO ()
main = print (banner, a, d)
