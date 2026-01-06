module Translations.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.FileSystem (FileSystem)
import Effects.FileSystem (RoFileSystem, readFileBS)

data TransTree
    = Branch (HashMap Text TransTree)
    | Leaf Text
    deriving (Show, Eq)

instance FromJSON TransTree where
    parseJSON :: Value -> Parser TransTree
    parseJSON (String s) = pure (Leaf s)
    parseJSON ob@(Object _) = Branch <$> parseJSON ob
    parseJSON e = typeMismatch "String or Object" e

parseTransTree :: ByteString -> Maybe TransTree
parseTransTree = decode . BL.fromStrict

readTranslations :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe TransTree)
readTranslations path = readFileBS path >>= pure . parseTransTree