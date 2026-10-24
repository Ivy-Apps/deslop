module Tiny.Types (Name (..)) where

import Data.Text (Text)
-- Only for its instances, it says; but it reaches the terminal.
import Tiny.Log ()

newtype Name = Name Text
