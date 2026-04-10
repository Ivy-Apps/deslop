module SecretsSpec (spec) where

import Effectful
import Effects.FileSystem
import Secrets
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "getSecrets" $ do
    it "missing secrets" $ do
        res <- runGetSecrets $ fixturesBasePath </> [osp|secrets|] </> [osp|missing|]
        res `shouldBe` Left MissingSecretsFile
    it "invalid secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> [osp|secrets|] </> [osp|invalid.json|])
        res `shouldSatisfy` \case
            Left (InvalidSecretsJson _) -> True
            _ -> False
    it "vaid secrets without a GeminiApiKey" $ do
        res <- runGetSecrets (fixturesBasePath </> [osp|secrets|] </> [osp|no-gemini-api-key.json|])
        let expected = Secrets {geminiApiKey = Nothing}
        res `shouldBe` Right expected
    it "valid secrets" $ do
        res <- runGetSecrets (fixturesBasePath </> [osp|secrets|] </> [osp|valid.json|])
        let expected = Secrets {geminiApiKey = Just $ GeminiApiKey "test-key"}
        res `shouldBe` Right expected

runGetSecrets :: OsPath -> IO (Either SecretsError Secrets)
runGetSecrets = runEff . runFileSystemIO . getSecrets
