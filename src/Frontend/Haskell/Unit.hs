{- | One Haskell file as read: its header and imports, or - when they cannot
be read - its bytes, kept whole so that rendering still gives them back and
the file is still a module 'Frontend.Haskell.Module' lowers.
-}
module Frontend.Haskell.Unit (
    HsUnit (..),
    Opacity (..),
    readUnit,
    renderUnit,
    unitPath,
) where

import Data.Text.Encoding qualified as TE
import FileSystem.Path (AbsPath)
import Frontend.Haskell.CST (HsProgram (..))
import Frontend.Haskell.Cabal (Component (..), Owner (..), ownerOf)
import Frontend.Haskell.Lexer (lexHaskell)
import Frontend.Haskell.Parser (parseHaskell)
import Frontend.Haskell.Preprocessor (Request, opaqueRequest, requestedInFile)
import Renderable qualified

data HsUnit
    = Readable HsProgram
    | Opaque AbsPath Opacity ByteString
    deriving stock (Show, Eq)

-- | Why a file's imports were not read.
data Opacity
    = NotUtf8
    | {- | A @module@ line that never reaches @where@, or an import that is
      not one: a file GHC would reject.
      -}
      Unparseable
    | -- | What GHC compiles is a preprocessor's output, not these bytes.
      Preprocessed Request
    deriving stock (Show, Eq)

{- | Total. The preprocessors asked for come first - by the file's own pragmas,
then by its component - because what CPP reads is not Haskell, and a guess at
its imports would be read as fact.
-}
readUnit :: [Component] -> AbsPath -> ByteString -> HsUnit
readUnit components path bytes = case TE.decodeUtf8' bytes of
    Left _ -> Opaque path NotUtf8 bytes
    Right text ->
        let tokens = lexHaskell text
            requests = requestedInFile tokens <> maybe [] (.component.preprocessors) (ownerOf components path)
         in case (opaqueRequest requests, parseHaskell tokens) of
                (Just request, _) -> Opaque path (Preprocessed request) bytes
                (Nothing, Nothing) -> Opaque path Unparseable bytes
                (Nothing, Just source) -> Readable HsProgram {path, source}

renderUnit :: HsUnit -> ByteString
renderUnit = \case
    Readable program -> TE.encodeUtf8 . Renderable.render $ program.source
    Opaque _ _ bytes -> bytes

unitPath :: HsUnit -> AbsPath
unitPath = \case
    Readable program -> program.path
    Opaque path _ _ -> path
