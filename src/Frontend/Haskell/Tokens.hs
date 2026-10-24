{- | The tokens Deslop splits Haskell source into.

Lossless: every byte of a file is in exactly one token, so concatenating
their 'raw' text gives the file back. Only as fine-grained as reading a module
header and its imports needs - a number, say, is a run of 'OtherK' tokens.
-}
module Frontend.Haskell.Tokens (
    HsToken (..),
    HsTokenKind (..),
    Positioned (..),
    positioned,
    isTrivia,
    isKeyword,
) where

import Data.List qualified as L
import Data.Text qualified as T
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
    | -- | @{\- ... -\}@, nested.
      BlockCommentK
    | -- | @{\-# ... #-\}@.
      PragmaK
    | {- | @foo@, @_x@, @x'@ - and the keywords @module@, @import@, @where@,
      which Haskell spells as variable identifiers.
      -}
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

-- | A token and where it starts, 1-based.
data Positioned = Positioned
    { line :: Int
    , column :: Int
    , token :: HsToken
    }

positioned :: [HsToken] -> [Positioned]
positioned = snd . L.mapAccumL place (1, 1)
  where
    place pos t = (T.foldl' advance pos t.raw, uncurry Positioned pos t)
    advance (l, c) = \case
        '\n' -> (l + 1, 1)
        -- GHC's tab stops are 8 columns apart.
        '\t' -> (l, ((c - 1) `div` 8 + 1) * 8 + 1)
        '\xFEFF' -> (l, c)
        _ -> (l, c + 1)

-- | Whitespace and comments: what layout and grammar look straight through.
isTrivia :: HsToken -> Bool
isTrivia token = token.kind `elem` [WhitespaceK, LineCommentK, BlockCommentK, PragmaK]

-- | Whether the token is this reserved or special word, e.g. @import@.
isKeyword :: Text -> HsToken -> Bool
isKeyword word token = token.kind == VarIdK && token.raw == word

instance Renderable HsToken where
    render = (.raw)
