{-# OPTIONS_GHC -Wno-orphans #-}

module Translations.Parser where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, listDirectory, readFileBS)
import System.FilePath
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
    = Branch (OMap Text TransTree)
    | Leaf Text
    deriving (Show, Eq)

instance FromJSON TransTree where
    parseJSON :: Value -> Parser TransTree
    parseJSON (String s) = pure (Leaf s)
    parseJSON ob@(Object _) = Branch <$> parseJSON ob
    parseJSON e = typeMismatch "String or Object" e

instance (FromJSON v) => FromJSON (OMap Text v) where
    parseJSON :: Value -> Parser (OMap Text v)
    parseJSON = withObject "OMap" $ \obj -> do
        traverse step (KM.toList obj) >>= pure . OM.fromList
      where
        step (k, v) = (,) (K.toText k) <$> parseJSON v

defaultLanguage :: Text
defaultLanguage = "en"

readTranslations :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe Translations)
readTranslations root =
    listDirectory root
        >>= traverse readTranslation . fmap (root </>)
        >>= pure . assemble . catMaybes
  where
    assemble :: [Translation] -> Maybe Translations
    assemble =
        uncurry (liftA2 Translations)
            . bimap safeHead nonEmpty
            . partition ((== defaultLanguage) . (.language))

readTranslation :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe Translation)
readTranslation path =
    readFileBS path
        >>= pure . fmap (Translation language) . parseTransTree
  where
    language = T.pack . takeBaseName $ path

parseTransTree :: ByteString -> Maybe TransTree
parseTransTree = decode . BL.fromStrict
