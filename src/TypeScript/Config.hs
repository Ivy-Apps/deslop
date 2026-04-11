module TypeScript.Config (
    parseTsConfigLegacy,
    TsConfigLegacy (..),
    ImportAlias (..),
    readTsConfig,
    parseTsConfig,
    parsePattern,
    TsConfig (..),
    PathMapping (..),
    Pattern (..),
) where

import Data.Aeson (FromJSON, decode, decode')
import Data.Map qualified as M
import Data.Text qualified as T
import Effectful
import Effects.FileSystem (AbsPath, RoFileSystem, encodeOsPath, fsMkAbsolute, fsReadAbsFile, withAbsBaseSafe)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Utils (safeHead)

newtype TsConfigDto = TsConfigDto
    { compilerOptions :: CompilerOptionsDto
    }
    deriving (Show, Generic)

data CompilerOptionsDto = CompilerOptionsDto
    { baseUrl :: Maybe Text
    , paths :: Maybe (Map Text [Text])
    }
    deriving (Show, Generic)

instance FromJSON TsConfigDto
instance FromJSON CompilerOptionsDto

data TsConfig = TsConfig
    { baseUrl :: !AbsPath
    , paths :: ![PathMapping]
    }
    deriving (Show, Eq)

data PathMapping = PathMapping
    { key :: !Pattern
    , value :: !(NonEmpty Pattern)
    }
    deriving (Show, Eq)

data Pattern
    = Exact !Text
    | WildCard {pre :: !Text, suff :: !Text}
    deriving (Show, Eq)

readTsConfig :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text TsConfig)
readTsConfig path = fsReadAbsFile path >>= parseTsConfig path

parseTsConfig :: (RoFileSystem :> es) => AbsPath -> ByteString -> Eff es (Either Text TsConfig)
parseTsConfig _p _json = pure $ Left "WIP"
  where
    _decodeJson :: ByteString -> Either Text TsConfigDto
    _decodeJson bs = do
        cleanJson <- bimap show stripTsComments . decodeUtf8' $ bs
        maybeToRight "Failed to parse JSON" . decode' @TsConfigDto . encodeUtf8 $ cleanJson

_mapToTsConfig :: (RoFileSystem :> es) => AbsPath -> TsConfigDto -> Eff es TsConfig
_mapToTsConfig path dto = do
    let baseUrl = encodeOsPath . fromMaybe "." $ dto.compilerOptions.baseUrl
    absBaseUrl <- fsMkAbsolute $ withAbsBaseSafe path baseUrl
    pure
        TsConfig
            { baseUrl = absBaseUrl
            , paths = []
            }

_parsePathMapping :: (Text, [Text]) -> Maybe PathMapping
_parsePathMapping _ =
    Just $
        PathMapping
            { key = Exact ""
            , value = Exact "" :| []
            }

parsePattern :: Text -> Maybe Pattern
parsePattern "" = Nothing
parsePattern t = case T.count "*" t of
    0 -> Just $ Exact t
    1 -> let (pre, suff) = T.breakOn "*" t in Just $ WildCard pre (T.drop 1 suff)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Comment Stripping Logic
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

-- | Safely strips // and /* */ comments from a JSON string.
stripTsComments :: Text -> Text
stripTsComments input = fromMaybe input . parseMaybe jsoncStripper $ input

jsoncStripper :: Parser Text
jsoncStripper =
    T.concat
        <$> many
            ( stringLiteral
                <|> try lineComment
                <|> try blockComment
                <|> otherText
                <|> slash
            )
  where
    -- Safely consume string literals to protect URLs like "http://..."
    stringLiteral :: Parser Text
    stringLiteral = do
        start <- chunk "\""
        inner <- many (try escapedChar <|> normalStringChar)
        end <- chunk "\""
        pure $ start <> T.concat inner <> end

    escapedChar :: Parser Text
    escapedChar = do
        esc <- char '\\'
        c <- anySingle
        pure $ T.pack [esc, c]

    normalStringChar :: Parser Text
    normalStringChar = takeWhile1P Nothing (\c -> c /= '"' && c /= '\\')

    -- Strip out line comments
    lineComment :: Parser Text
    lineComment = do
        _ <- chunk "//"
        _ <- takeWhileP Nothing (/= '\n')
        pure ""

    -- Strip out block comments
    blockComment :: Parser Text
    blockComment = do
        _ <- chunk "/*"
        _ <- manyTill anySingle (chunk "*/")
        pure ""

    -- Bulk consume safe characters for performance
    otherText :: Parser Text
    otherText = takeWhile1P Nothing (\c -> c /= '"' && c /= '/')

    -- Catchall for isolated slashes
    slash :: Parser Text
    slash = chunk "/"

--------------------
-- LEGACY CODE
--------------------

newtype TsConfigLegacy = TsConfigLegacy
    { paths :: [ImportAlias]
    }
    deriving (Show, Eq)

data ImportAlias = ImportAlias
    { label :: Text
    , path :: Text
    }
    deriving (Show, Eq)

-- | Parses the TSConfig, safely stripping comments before passing to Aeson
parseTsConfigLegacy :: ByteString -> Maybe TsConfigLegacy
parseTsConfigLegacy = fromJson >=> extractPaths >=> pure . buildConfig
  where
    fromJson :: ByteString -> Maybe TsConfigDto
    fromJson =
        rightToMaybe
            . decodeUtf8'
            >=> decode @TsConfigDto
            . encodeUtf8
            . stripTsComments

    extractPaths :: TsConfigDto -> Maybe (Map Text [Text])
    extractPaths = (.paths) . (.compilerOptions)

    buildConfig :: Map Text [Text] -> TsConfigLegacy
    buildConfig =
        TsConfigLegacy
            . sortByLongest
            . mapMaybe parseAlias
            . M.toList
            . M.mapMaybe safeHead

    parseAlias :: (Text, Text) -> Maybe ImportAlias
    parseAlias = Just . uncurry ImportAlias . join bimap cleanPath

    cleanPath :: Text -> Text
    cleanPath = (fromMaybe <*> T.stripPrefix "./") . T.takeWhile (/= '*')

    sortByLongest :: [ImportAlias] -> [ImportAlias]
    sortByLongest = sortOn (Down . T.length . (.label))
