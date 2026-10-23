module Tiny.C (c) where

import qualified Tiny.A

c :: Int
c = length (show Tiny.A.a)
