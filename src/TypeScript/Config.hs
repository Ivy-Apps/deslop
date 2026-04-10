module TypeScript.Config (
    parseTsConfigLegacy,
    TsConfigLegacy (..),
    ImportAlias (..),
) where

import Data.Aeson (FromJSON, decode)
import Data.Map qualified as M
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Utils (safeHead)

newtype TsConfigDto = TsConfigDto
    { compilerOptions :: CompilerOptionsDto
    }
    deriving (Show, Generic)

newtype CompilerOptionsDto = CompilerOptionsDton
    { paths :: Maybe (Map Text [Text])
    }
    deriving (Show, Generic)

instance FromJSON TsConfigDto
instance FromJSON CompilerOptionsDto

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
