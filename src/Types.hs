module Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Params = Params
    { projectPath :: FilePath
    , imports :: Bool
    , comments :: Bool
    , modified :: Bool
    }
    deriving (Show, Eq)

data Secrets = Secrets
    { geminiApiKey :: Text
    }
    deriving (Show, Eq, FromJSON, Generic)

data DeslopError
    = TsConfigNotFoundError FilePath
    | TsConfigParseError FilePath
    deriving (Show, Eq)

data TranslationsError
    = ParseTranslationsError
    | TranslateError Text
    deriving (Show, Eq)
