module Monetization.PaywallSpec (spec) where

import Doubles.CLI (MockCLI (..), defaultMockCLI, runMockCLI)
import Doubles.Polar (MockPolar (..), defaultMockPolar, runMockPolar)
import Doubles.Random (runMockRandom)
import Doubles.System (MockSystem (..), defaultMockSystem, runMockSystem)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.Polar (LicenseError (..), LicenseKey (..))
import Monetization.Paywall (paywallCheck)
import Test.Hspec (Spec, describe, it, shouldBe)
import Types (DeslopError)
import Types qualified as TP

spec :: Spec
spec = describe "Monetization.Paywall" $ do
    describe "Terminal" $ do
        it "no license should allow and not call Polar" $ do
            res <-
                runEff
                    . runMockSystem defaultMockSystem {mockIsTerminal = True}
                    . runMockPolar defaultMockPolar
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Right ()

        it "valid/invalid license should allow and not call Polar" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = True
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                _ -> Nothing
                            }
                    . runMockPolar defaultMockPolar
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Right ()

    describe "CI" $ do
        it "no license should block" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Nothing
                                "CI" -> Just "true" -- Detect CI
                                _ -> Nothing
                            }
                    . runMockPolar defaultMockPolar
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Left TP.NoLicenseError

        it "valid license should allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "valid"
                                "CI" -> Just "true" -- Detect CI
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "valid" -> Right ()
                                _ -> Left InvalidLicenseError
                            }
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Right ()

        it "invalid license should block" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "invalid"
                                "CI" -> Just "true" -- Detect CI
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "invalid" -> Left InvalidLicenseError
                                _ -> error "unexpected polar input"
                            }
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Left TP.InvalidLicenseError

        it "valid/invalid license but rate limitted should allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                "CI" -> Just "true" -- Detect CI
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                _ -> Left RateLimitError
                            }
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Right ()

        it "usage exceeded should block" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                "CI" -> Just "true" -- Detect CI
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                _ -> Left UsageExceededError
                            }
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Left TP.UsageExceededError

        it "connectivity/generic error should block" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                "CI" -> Just "true" -- Detect CI
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                _ -> Left GenericError
                            }
                    . runMockCLI defaultMockCLI
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom []
                    $ paywallCheck
            res `shouldBe` Left TP.LicenseGenericError

    describe "Maybe CI" $ do
        it "no license should captcha" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Nothing
                                _ -> Nothing
                            }
                    . runMockPolar defaultMockPolar
                    . runMockCLI defaultMockCLI {mockReadLines = ["incorrect"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Left TP.CaptchaError

        it "valid license show allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "valid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "valid" -> Right ()
                                _ -> Left InvalidLicenseError
                            }
                    . runMockCLI defaultMockCLI {mockReadLines = ["incorrect"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Right ()

        it "invalid license should captcha" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "invalid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "invalid" -> Left InvalidLicenseError
                                _ -> error "Unexpected Polar test input"
                            }
                    . runMockCLI defaultMockCLI {mockReadLines = ["incorrect"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Left TP.CaptchaError

        it "valid/invalid license but rate limitted should allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "license" -> Left RateLimitError
                                _ -> error "Unexpected Polar test input"
                            }
                    . runMockCLI defaultMockCLI {mockReadLines = ["incorrect"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Right ()

        it "usage exceeded should captcha" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "license" -> Left UsageExceededError
                                _ -> error "Unexpected Polar test input"
                            }
                    . runMockCLI defaultMockCLI {mockReadLines = ["incorrect"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Left TP.CaptchaError

        it "connectivity error should captcha" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "license"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                LicenseKey "license" -> Left GenericError
                                _ -> error "Unexpected Polar test input"
                            }
                    . runMockCLI defaultMockCLI {mockReadLines = ["incorrect"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Left TP.CaptchaError

        it "no licenses but passes captcha should allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Nothing
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { mockCheckLicense = \case
                                _ -> error "Polar must not be called"
                            }
                    . runMockCLI defaultMockCLI {mockReadLines = ["4"]}
                    . runErrorNoCallStack @DeslopError
                    . runMockRandom [0, 2, 2]
                    $ paywallCheck
            res `shouldBe` Right ()
