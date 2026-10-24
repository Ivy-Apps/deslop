{- | One Nix file as read: its lexemes, or - when they cannot be read - its
bytes, kept whole so that rendering still gives them back and the file is
still a module "Frontend.Nix.Module" lowers.
-}
module Frontend.Nix.Unit (
    NixUnit (..),
    NixSource (..),
    Opacity (..),
    readUnit,
    renderUnit,
    unitPath,
) where

import Data.Text.Encoding qualified as TE
import FileSystem.Path (AbsPath)
import Frontend.Nix.Lexeme (Lexeme)
import Frontend.Nix.Lexer (lexNix)
import Renderable qualified

data NixUnit
    = Readable NixSource
    | Opaque AbsPath Opacity ByteString
    deriving stock (Show, Eq)

data NixSource = NixSource
    { path :: AbsPath
    , lexemes :: [Lexeme]
    }
    deriving stock (Show, Eq)

-- | Why a file's references were not read.
data Opacity
    = NotUtf8
    | {- | An unterminated string or comment, an interpolation never closed:
      text Nix would not read either.
      -}
      Unlexable
    deriving stock (Show, Eq)

-- | Total.
readUnit :: AbsPath -> ByteString -> NixUnit
readUnit path bytes = case TE.decodeUtf8' bytes of
    Left _ -> Opaque path NotUtf8 bytes
    Right text -> maybe (Opaque path Unlexable bytes) (Readable . NixSource path) (lexNix text)

renderUnit :: NixUnit -> ByteString
renderUnit = \case
    Readable source -> TE.encodeUtf8 . Renderable.render $ source.lexemes
    Opaque _ _ bytes -> bytes

unitPath :: NixUnit -> AbsPath
unitPath = \case
    Readable source -> source.path
    Opaque path _ _ -> path
