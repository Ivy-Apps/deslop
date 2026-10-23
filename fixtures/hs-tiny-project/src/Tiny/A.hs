module Tiny.A (a) where

import Data.Text qualified as T
import Tiny.B (b)

a :: T.Text
a = T.pack (show b)
