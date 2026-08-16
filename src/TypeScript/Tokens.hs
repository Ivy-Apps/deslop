module TypeScript.Tokens (
    TsToken (..),
    TsTokenKind (..),
) where

data TsToken = TsToken
    { raw :: Text
    , kind :: TsTokenKind
    }
    deriving (Show, Eq)

{- | Comments carry no content: they are lexed only so that an @import@ inside
one is never mistaken for a real import.
-}
data TsTokenKind
    = ImportK
    | CommentK
    | WhitespaceK
    | RawK
    deriving (Show, Eq)

