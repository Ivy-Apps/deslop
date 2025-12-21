module TypeScript.Ast where

import Data.Text

data TsAst
  = Import
      { path :: FilePath
      , raw :: Text
      }
  | Comment
      { text :: Text
      , raw :: Text
      }
  | Source {raw :: Text}
  deriving (Show, Eq)