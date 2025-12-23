module TypeScript.Tokens where

import Data.Text (Text)

data TsToken = TsToken
    { raw :: Text
    , kind :: TsTokenKind
    }
    deriving (Show, Eq)

data TsTokenKind
    = ImportK
    | CommentK {comment :: Text}
    | DocsK {comment :: Text}
    | RawK
    deriving (Show, Eq)