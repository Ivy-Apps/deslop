module Translations.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, listDirectory, readFileBS)
import System.FilePath
import System.FilePath (dropExtension, splitDirectories)
import Utils (safeHead)

data Translations = Translations
    { base :: Translation
    , extra :: NonEmpty Translation
    }
    deriving (Show, Eq)

data Translation = Translation
    { language :: Text
    , tree :: TransTree
    }
    deriving (Show, Eq)

data TransTree
    = Branch (HashMap Text TransTree)
    | Leaf Text
    deriving (Show, Eq)

instance FromJSON TransTree where
    parseJSON :: Value -> Parser TransTree
    parseJSON (String s) = pure (Leaf s)
    parseJSON ob@(Object _) = Branch <$> parseJSON ob
    parseJSON e = typeMismatch "String or Object" e

defaultLanguage :: Text
defaultLanguage = "en"

readTranslations :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe Translations)
readTranslations root = do
    ds <- fmap (root </>) <$> listDirectory root
    ts <- catMaybes <$> traverse readTranslation ds
    let base = safeHead . filter (isDeault . (.language)) $ ts
    let extras = nonEmpty . filter (not . isDeault . (.language)) $ ts
    pure $ Translations <$> base <*> extras
  where
    isDeault = (==) defaultLanguage

readTranslation :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe Translation)
readTranslation path =
    readFileBS path
        >>= pure . fmap (Translation language) . parseTransTree
  where
    language = T.pack . dropExtension . last . splitDirectories $ path

parseTransTree :: ByteString -> Maybe TransTree
parseTransTree = decode . BL.fromStrict
