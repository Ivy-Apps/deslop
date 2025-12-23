module TypeScript.Tokens where

import Data.Text (Text)

data TsToken
  = ImportTok
      { statement :: Text
      , raw :: Text
      }
  | CommentTok
      { comment :: Text
      , raw :: Text
      }
  | RawTok
      { raw :: Text
      }