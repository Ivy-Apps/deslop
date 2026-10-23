module Tiny.B (b) where

import {-# SOURCE #-} Tiny.C (c)

b :: Int
b = c + 1
