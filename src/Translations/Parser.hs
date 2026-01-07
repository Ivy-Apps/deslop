{-# OPTIONS_GHC -Wno-orphans #-}
module Translations.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, listDirectory, readFileBS)
import System.FilePath
import Utils (safeHead)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM

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
        -- KM.toList extracts [(Key, Value)]. 
        -- In Aeson 2+, this generally respects the order of the keys in the source JSON.
        let rawPairs = KM.toList obj
        
        -- Traverse the list, converting Key to Text and parsing the Value
        parsedPairs <- traverse (\(k, v) -> (,) (K.toText k) <$> parseJSON v) rawPairs
        
        -- Construct the Ordered Map
        pure $ OMap.fromList parsedPairs

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
