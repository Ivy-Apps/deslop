module Main (main) where

import Tiny.A (a)
import "hs-tiny" Tiny.D (d)

main :: IO ()
main = print (a, d)
