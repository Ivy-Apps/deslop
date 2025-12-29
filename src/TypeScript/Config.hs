module TypeScript.Config where

import Data.Aeson
import Data.Map
import Data.Text
import GHC.Generics

data TsConfig = TsConfig
    { compilerOptions :: CompilerOptions
    }
    deriving (Show, Generic)

data CompilerOptions = CompilerOptions
    { baseUrl :: Maybe FilePath
    , paths :: Maybe (Map Text [Text])
    }
    deriving (Show, Generic)

instance FromJSON TsConfig
instance FromJSON CompilerOptions