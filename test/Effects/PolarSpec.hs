module Effects.PolarSpec (spec) where

import Effectful (runEff)
import Effects.Polar (LicenseError (InvalidLicenseError, UsageExceededError), LicenseKey (LicenseKey), pCheckLicense, runPolar)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (requireEnvVar)

spec :: Spec
spec = describe "Effects.Polar" $ do
    describe "pCheckLicense" $ do
        it "valid license" $ do
            license <- LicenseKey <$> requireEnvVar "DESLOP_ULTRA_TEST_KEY"
            res <- runEff . runPolar $ pCheckLicense license
            res `shouldBe` Right ()
        it "usage exceeded" $ do
            license <- LicenseKey <$> requireEnvVar "DESLOP_ONE_TEST_KEY"
            res <- runEff . runPolar $ pCheckLicense license
            res `shouldBe` Left UsageExceededError
        it "invalid license" $ do
            res <- runEff . runPolar $ pCheckLicense (LicenseKey "invalid")
            res `shouldBe` Left InvalidLicenseError
            True `shouldBe` True
