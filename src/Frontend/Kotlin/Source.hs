{- | SPIKE: a crude, lossless Kotlin "parser" that understands only the file
header - the @package@ directive and the @import@ list - and keeps every line
verbatim, so rendering gives back the exact bytes.

Not a Kotlin parser: block comments, string templates spanning lines and
annotations before @package@ are all read as 'Other'.
-}
module Frontend.Kotlin.Source (
    KtFile (..),
    KtLine (..),
    KtImport (..),
    KtParseError (..),
    parseKt,
    renderKt,
    packageOf,
    importsOf,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import FileSystem.Path (AbsPath (osPath), decodeOsPath)
import Renderable (Renderable (..))

-- | What @import a.b.C as D@ or @import a.b.*@ says.
data KtImport = KtImport
    { path :: [Text]
    -- ^ @a.b.C@ split on dots; for a wildcard, the package itself.
    , wildcard :: Bool
    }
    deriving stock (Show, Eq)

-- | One physical line, classified, and always kept as written.
data KtLine
    = PackageLine [Text] Text
    | ImportLine KtImport Text
    | Other Text
    deriving stock (Show, Eq)

data KtFile = KtFile
    { path :: AbsPath
    , body :: [KtLine]
    }
    deriving stock (Show, Eq)

newtype KtParseError = KtParseError Text
    deriving stock (Show, Eq)

instance Renderable KtParseError where
    render (KtParseError err) = err

parseKt :: AbsPath -> ByteString -> Either KtParseError KtFile
parseKt path bytes = case TE.decodeUtf8' bytes of
    Left _ -> Left . KtParseError $ decodeOsPath path.osPath <> ": not UTF-8"
    Right txt -> Right . KtFile path . fmap classify . T.splitOn "\n" $ txt

renderKt :: KtFile -> ByteString
renderKt = TE.encodeUtf8 . T.intercalate "\n" . fmap raw . (.body)

raw :: KtLine -> Text
raw = \case
    PackageLine _ t -> t
    ImportLine _ t -> t
    Other t -> t

classify :: Text -> KtLine
classify line = case T.words . T.dropWhileEnd (== ';') . T.strip $ line of
    ["package", fqn] -> PackageLine (dotted fqn) line
    ("import" : fqn : _) -> ImportLine (importOf fqn) line
    _ -> Other line
  where
    importOf fqn = case T.stripSuffix ".*" fqn of
        Just pkg -> KtImport (dotted pkg) True
        Nothing -> KtImport (dotted fqn) False
    dotted = T.splitOn "."

-- | The package a file declares; the default (root) package when it has none.
packageOf :: KtFile -> [Text]
packageOf file = fromMaybe [] . listToMaybe $ [p | PackageLine p _ <- file.body]

-- | Every import, with its 1-based line and verbatim text.
importsOf :: KtFile -> [(Int, KtImport, Text)]
importsOf file = [(n, i, t) | (n, ImportLine i t) <- zip [1 ..] file.body]
