module TypeScript.Tokens (
    TsToken (..),
    TsTokenKind (..),
) where

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

