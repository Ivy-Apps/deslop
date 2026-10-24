-- | The domain: pure, and re-exporting its types.
module Tiny.Core (
    module Tiny.Types,
    greet,
) where

import Data.Text qualified as T
import Tiny.Types

greet :: Name -> T.Text
greet (Name n) = "Hello, " <> n
