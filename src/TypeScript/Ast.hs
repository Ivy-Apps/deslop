module TypeScript.Ast where

import Data.Text (Text)

data TsProgram = TsFile
  { path :: FilePath
  , name :: Text
  , nodes :: [TsNode]
  }
  deriving (Show, Eq)

data TsNode
  = -- | A TypeScript Import Declaration (e.g., @import { A } from './b';@).
    --
    -- The parser splits this into a "prefix" (everything before the path)
    -- and the "target" (the module path itself).
    Import
      { prefix :: Text
      -- ^ "import { A } from " OR "const x = await import("
      , target :: Text
      -- ^ "'./utils'" (Always includes the '' quotes)
      , suffix :: Text
      -- ^ ";" OR ")"
      -- Captures the closing syntax so it moves with the import.
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