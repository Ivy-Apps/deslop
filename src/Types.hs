module Types (
    Secrets (..),
    DeslopError (..),
    TranslationsError (..),
    InitError (..),
    Renderable(..)
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

class Renderable a where
  render :: a -> Text

instance Renderable a => Renderable [a] where
  render = foldl' (\acc x -> acc <> render x) ""

newtype Secrets = Secrets
    { geminiApiKey :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data DeslopError
    = TsConfigNotFoundError FilePath
    | TsConfigParseError FilePath
    deriving (Show, Eq)

data TranslationsError
    = ParseTranslationsError
    | TranslateError Text
    deriving (Show, Eq)

data InitError = SecretsMissing | SecretsJsonError Text deriving (Show, Eq)
