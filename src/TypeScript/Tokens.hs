module TypeScript.Tokens where

import Data.Text (Text)

data TsProgram = TsModule
    { path :: FilePath
    , tokens :: [TsToken]
    }
    deriving (Show, Eq)

data TsToken = TsToken
    { raw :: Text
    , kind :: TsTokenKind
    }
    deriving (Show, Eq)

data TsTokenKind
    = ImportK
    | CommentK {comment :: Text}
    | DocsK {comment :: Text}
    | WhitespaceK
    | RawK
    deriving (Show, Eq)