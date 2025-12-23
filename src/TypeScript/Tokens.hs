module TypeScript.Tokens where

import Data.Text (Text)

data TsToken
  = ImportTok
      { raw :: Text
      }
  | CommentTok
      { comment :: Text
      , raw :: Text
      }
  | RawTok
      { raw :: Text
      }
  deriving (Show, Eq)