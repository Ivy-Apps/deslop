module SecretsSpec (spec) where

import Effectful
import Effects.FileSystem
import FsEncoding (encodePathString)
import Secrets
import System.FilePath
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "getSecrets" $ do
    it "missing secrets" $ do
        res <- runGetSecrets $ fixturesBasePath </> "secrets" </> "missing"
        res `shouldBe` Left MissingSecretsFile
    it "invalid secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> "secrets" </> "invalid.json")
        res `shouldSatisfy` \case
            Left (InvalidSecretsJson _) -> True
            _ -> False
    it "vaid secrets without a GeminiApiKey" $ do
        res <- runGetSecrets (fixturesBasePath </> "secrets" </> "no-gemini-api-key.json")
        let expected = Secrets {geminiApiKey = Nothing}
        res `shouldBe` Right expected
    it "valid secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> "secrets" </> "valid.json")
        let expected = Secrets {geminiApiKey = Just $ GeminiApiKey "test-key"}
        res `shouldBe` Right expected

runGetSecrets :: FilePath -> IO (Either SecretsError Secrets)
runGetSecrets fp = runEff . runFileSystemIO . getSecrets $ encodePathString fp
