module TypeScript.Config (
    readTsConfig,
    parsePattern,
    parsePathMapping,
    TsConfig (..),
    PathMapping (..),
    Pattern (..),
    KeyPattern (..),
    ValuePattern (..),
) where

import Data.Aeson (FromJSON, decode')
import Data.Map qualified as M
import Data.Text qualified as T
import Effectful
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe, encodeOsPath, fsMkAbsolute, fsReadAbsFile, withAbsBaseSafe)
import System.OsPath (takeDirectory)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

data TsConfig = TsConfig
    { baseUrl :: !AbsPath
    , paths :: ![PathMapping]
    }
    deriving (Show, Eq)

data PathMapping = PathMapping
    { key :: !KeyPattern
    , values :: !(NonEmpty ValuePattern)
    }
    deriving (Show, Eq)

newtype KeyPattern = KeyPattern
    { pattern :: Pattern
    }
    deriving (Show, Eq)
newtype ValuePattern = ValuePattern
    { pattern :: Pattern
    }
    deriving (Show, Eq)

data Pattern
    = Exact !Text
    | Wildcard {pre :: !Text, suff :: !Text}
    deriving (Show, Eq)

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

readTsConfig :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text TsConfig)
readTsConfig cfgPath = fsReadAbsFile cfgPath >>= parseTsConfigFromJson cfgPath

parseTsConfigFromJson :: (RoFileSystem :> es) => AbsPath -> ByteString -> Eff es (Either Text TsConfig)
parseTsConfigFromJson cfgPath json = do
    case decodeJson json of
        Right dto -> Right <$> parseTsConfig cfgPath dto
        Left err -> pure . Left $ err
  where
    decodeJson :: ByteString -> Either Text TsConfigDto
    decodeJson bs = do
        cleanJson <- bimap show stripTsComments . decodeUtf8' $ bs
        maybeToRight ("Invalid TSConfig JSON: " <> show cfgPath.osPath)
            . decode' @TsConfigDto
            . encodeUtf8
            $ cleanJson

parseTsConfig :: (RoFileSystem :> es) => AbsPath -> TsConfigDto -> Eff es TsConfig
parseTsConfig cfgPath dto = do
    let baseUrl = encodeOsPath . fromMaybe "." $ dto.compilerOptions.baseUrl
    let cfgDir = absPathUnsafe . takeDirectory $ cfgPath.osPath
    absBaseUrl <- fsMkAbsolute $ withAbsBaseSafe cfgDir baseUrl
    pure
        TsConfig
            { baseUrl = absBaseUrl
            , paths =
                sortPathMappings
                    . mapMaybe parsePathMapping
                    . M.toList
                    . fromMaybe mempty
                    $ dto.compilerOptions.paths
            }

sortPathMappings :: [PathMapping] -> [PathMapping]
sortPathMappings = sortOn (Down . patternSortKey . extractPattern . (.key))
  where
    extractPattern :: KeyPattern -> Pattern
    extractPattern (KeyPattern p) = p
    -- 'Down' reverses the default ascending sort, meaning higher numbers come first.
    patternSortKey :: Pattern -> (Int, Int, Int)
    patternSortKey (Exact k) =
        -- Priority 1: Exact matches always float to the top.
        (1, T.length k, 0)
    patternSortKey (Wildcard pre suff) =
        -- Priority 0: Wildcards come after Exact matches.
        -- They are sub-sorted by prefix length, then suffix length.
        (0, T.length pre, T.length suff)

parsePathMapping :: (Text, [Text]) -> Maybe PathMapping
parsePathMapping (_, []) = Nothing
parsePathMapping (k, vs) = do
    key <- parsePattern k
    values <-
        nonEmpty
            . fmap cleanValuePattern
            . filter (validKeyValuePair key)
            . mapMaybe parsePattern
            $ vs
    Just
        PathMapping
            { key = KeyPattern key
            , values = ValuePattern <$> values
            }
  where
    cleanValuePattern :: Pattern -> Pattern
    cleanValuePattern (Exact t) = Exact (cleanPrefix t)
    cleanValuePattern (Wildcard pre suff) = Wildcard (cleanPrefix pre) suff

    cleanPrefix :: Text -> Text
    cleanPrefix t
        | t == "." = ""
        | Just rest <- T.stripPrefix "./" t = cleanPrefix rest
        | otherwise = t

    validKeyValuePair :: Pattern -> Pattern -> Bool
    validKeyValuePair (Exact _) (Wildcard _ _) = False
    validKeyValuePair _ _ = True

parsePattern :: Text -> Maybe Pattern
parsePattern "" = Nothing
parsePattern t = case T.count "*" t of
    0 -> Just $ Exact t
    1 -> let (pre, suff) = T.breakOn "*" t in Just $ Wildcard pre (T.drop 1 suff)
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
