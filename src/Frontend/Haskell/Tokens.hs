{- | The tokens Deslop splits Haskell source into.

Lossless: every byte of a file is in exactly one token, so concatenating
their 'raw' text gives the file back. Only as fine-grained as reading a module
header and its imports needs - a number, say, is a run of 'OtherK' tokens.
-}
module Frontend.Haskell.Tokens (
    HsToken (..),
    HsTokenKind (..),
    isTrivia,
    isKeyword,
) where

import Renderable (Renderable (..))

data HsToken = HsToken
    { raw :: Text
    , kind :: HsTokenKind
    }
    deriving stock (Show, Eq, Ord)

data HsTokenKind
    = -- | Spaces, tabs, newlines and a byte-order mark.
      WhitespaceK
    | -- | @-- ...@ to the end of the line, or a @#!@ line opening the file.
      LineCommentK
    | -- | @{- ... -}@, nested.
      BlockCommentK
    | -- | @{-# ... #-}@.
      PragmaK
    | -- | @foo@, @_x@, @x'@ - and the keywords @module@, @import@, @where@,
      -- which Haskell spells as variable identifiers.
      VarIdK
    | -- | @Map@, or a dotted run of them, @Data.Map.Strict@.
      ConIdK
    | -- | A run of symbol characters that is not a comment: @+@, @-->@, @--|@.
      SymbolK
    | StringK
    | CharK
    | -- | One of @( ) , ; [ ] \` { }@.
      SpecialK
    | -- | Anything else, one character at a time.
      OtherK
    deriving stock (Show, Eq, Ord)

-- | Whitespace and comments: what layout and grammar look straight through.
isTrivia :: HsToken -> Bool
isTrivia token = token.kind `elem` [WhitespaceK, LineCommentK, BlockCommentK, PragmaK]

-- | Whether the token is this reserved or special word, e.g. @import@.
isKeyword :: Text -> HsToken -> Bool
isKeyword word token = token.kind == VarIdK && token.raw == word

instance Renderable HsToken where
    render = (.raw)
