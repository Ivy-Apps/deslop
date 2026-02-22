module DeslopSpec (spec) where

import Deslop
import Effectful
import Effects.FileSystem
import System.FilePath
import Test.Hspec
import TestUtils
import Types

spec :: Spec
spec = describe "getSecrets" $ do
    it "missing secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> "secrets" </> "missing")
        res `shouldBe` Left SecretsMissing
    it "invalid secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> "secrets" </> "invalid.json")
        res `shouldSatisfy` \case
            Left (SecretsJsonError _) -> True
            _ -> False
    it "valid secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> "secrets" </> "valid.json")
        let expected = Secrets {geminiApiKey = "test-key"}
        res `shouldBe` Right expected

runGetSecrets :: FilePath -> IO (Either InitError Secrets)
runGetSecrets = runEff . runFileSystemIO . getSecrets
