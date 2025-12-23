module TypeScript.Ast where

import Data.Text ( Text )

data TsNode
  = -- | A TypeScript Import Declaration (e.g., @import { A } from './b';@).
    --
    -- The parser splits this into a "prefix" (everything before the path)
    -- and the "target" (the module path itself).
    Import
      { prefix :: Text
      -- ^ Everything before the target module e.g. @"import { User } from "@
      , target :: Text
      -- ^ The target module e.g. @'../lib/utils', 'react'@
      , raw :: Text
      }
  | Comment
      { comment :: Text
      -- ^ The content inside the comment markers.
      , raw :: Text
      }
  | Source
      { raw :: Text
      }
  deriving (Show, Eq)