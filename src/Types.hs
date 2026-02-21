module Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Params = Params
    { projectPath :: FilePath
    , modifiedOnly :: Bool
    }
    deriving (Show, Eq)

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
