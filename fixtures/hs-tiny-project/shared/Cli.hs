module Cli (banner) where

import Data.Text qualified as T
import Tiny.B (b)

banner :: T.Text
banner = T.pack ("tiny " <> show b)
