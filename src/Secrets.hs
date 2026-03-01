module Secrets (
    Secrets (..),
    GeminiApiKey (..),
    SecretsError (..),
    defaultSecrets,
    getSecrets,
    readSecrets,
) where

import Data.Aeson
import Data.Bifunctor
import Data.Bool
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effects.FileSystem (RoFileSystem, fileExists, getHomeDirectory, readFileBS)
import GHC.Generics (Generic)
import System.FilePath ((</>))

secretsPath :: (RoFileSystem :> es) => Eff es FilePath
secretsPath = do
    home <- getHomeDirectory
    pure $ home </> ".deslop" </> "secrets.json"

newtype Secrets = Secrets
    { geminiApiKey :: Maybe GeminiApiKey
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

newtype GeminiApiKey = GeminiApiKey Text
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data SecretsError = MissingSecretsFile | InvalidSecretsJson Text deriving stock (Show, Eq)

defaultSecrets :: Secrets
defaultSecrets =
    Secrets
        { geminiApiKey = Nothing
        }

readSecrets :: (RoFileSystem :> es) => Eff es (Either SecretsError Secrets)
readSecrets = secretsPath >>= getSecrets

getSecrets :: (RoFileSystem :> es) => FilePath -> Eff es (Either SecretsError Secrets)
getSecrets sp =
    fileExists sp
        >>= bool (pure . Left $ MissingSecretsFile) readSecretsFile
  where
    readSecretsFile =
        readFileBS sp
            <&> first (InvalidSecretsJson . T.pack)
                . eitherDecode @Secrets
                . BL.fromStrict
