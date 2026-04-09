module TypeScript.Config (
    parseTsConfig,
    TsConfig (..),
    ImportAlias (..),
) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Text as T (length, stripPrefix, takeWhile)
import Utils (safeHead)

newtype TsConfigJson = TsConfigJson
    { compilerOptions :: CompilerOptionsJson
    }
    deriving (Show, Generic)

newtype CompilerOptionsJson = CompilerOptionsJson
    { paths :: Maybe (Map Text [Text])
    }
    deriving (Show, Generic)

instance FromJSON TsConfigJson
instance FromJSON CompilerOptionsJson

newtype TsConfig = TsConfig
    { paths :: [ImportAlias]
    }
    deriving (Show, Eq)

data ImportAlias = ImportAlias
    { label :: Text
    , path :: Text
    }
    deriving (Show, Eq)

parseTsConfig :: ByteString -> Maybe TsConfig
parseTsConfig = fromJson >=> extractPaths >=> pure . buildConfig
  where
    fromJson :: ByteString -> Maybe TsConfigJson
    fromJson = decode . BL.fromStrict

    extractPaths :: TsConfigJson -> Maybe (Map Text [Text])
    extractPaths = (.paths) . (.compilerOptions)

    buildConfig :: Map Text [Text] -> TsConfig
    buildConfig =
        TsConfig
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
