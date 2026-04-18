{-# LANGUAGE QuasiQuotes #-}

module Secrets (
    Secrets (..),
    GeminiApiKey (..),
    SecretsError (..),
    defaultSecrets,
    getSecrets,
    readSecrets,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Effectful
import Effects.FileSystem (AbsPath, RoFileSystem, fsFileExists, fsGetHomeDirectory, fsReadFile, withAbsBaseUnsafe)
import System.OsPath (osp)

secretsPath :: (RoFileSystem :> es) => Eff es AbsPath
secretsPath = do
    home <- fsGetHomeDirectory
    pure $ withAbsBaseUnsafe home [osp|.deslop/secrets.json|]

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

getSecrets :: (RoFileSystem :> es) => AbsPath -> Eff es (Either SecretsError Secrets)
getSecrets sp =
    fsFileExists sp
        >>= bool (pure . Left $ MissingSecretsFile) readSecretsFile
  where
    readSecretsFile =
        fsReadFile sp
            <&> first (InvalidSecretsJson . T.pack)
            . eitherDecode @Secrets
            . BL.fromStrict
