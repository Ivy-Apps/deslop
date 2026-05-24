module Monetization.CIDetectionSpec (spec) where

import Doubles.System (MockSystem (..), defaultMockSystem, runMockSystem)
import Effectful (runPureEff)
import Monetization.CIDetection (DetectedEnv (..), detectEnv)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Monetization.CIDetection" $ do
    describe "detectEnv" $ do
        it "returns CI when the CI env var is set" $ do
            let mocks =
                    defaultMockSystem
                        { mockLookupEnv = \case
                            "CI" -> Just "true"
                            _ -> Nothing
                        }
            let result = runPureEff . runMockSystem mocks $ detectEnv
            result `shouldBe` CI

        it "returns CI when GITHUB_ACTIONS env var is set" $ do
            let mocks =
                    defaultMockSystem
                        { mockLookupEnv = \case
                            "GITHUB_ACTIONS" -> Just "true"
                            _ -> Nothing
                        }
            let result = runPureEff . runMockSystem mocks $ detectEnv
            result `shouldBe` CI

        it "returns CI when TERM=dumb and no CI vars are set" $ do
            let mocks =
                    defaultMockSystem
                        { mockLookupEnv = \case
                            "TERM" -> Just "dumb"
                            _ -> Nothing
                        }
            let result = runPureEff . runMockSystem mocks $ detectEnv
            result `shouldBe` CI

        it "returns Terminal when isTerminal is True and no CI signals" $ do
            let mocks =
                    defaultMockSystem
                        { mockIsTerminal = True
                        }
            let result = runPureEff . runMockSystem mocks $ detectEnv
            result `shouldBe` Terminal

        it "returns MaybeCI when no CI vars, TERM is not dumb, and not a terminal" $ do
            let result = runPureEff . runMockSystem defaultMockSystem $ detectEnv
            result `shouldBe` MaybeCI

        it "returns CI (not Terminal) when a CI var is set and isTerminal is True" $ do
            let mocks =
                    defaultMockSystem
                        { mockLookupEnv = \case
                            "CI" -> Just "true"
                            _ -> Nothing
                        , mockIsTerminal = True
                        }
            let result = runPureEff . runMockSystem mocks $ detectEnv
            result `shouldBe` CI

        it "returns CI (not Terminal) when TERM=dumb and isTerminal is True" $ do
            let mocks =
                    defaultMockSystem
                        { mockLookupEnv = \case
                            "TERM" -> Just "dumb"
                            _ -> Nothing
                        , mockIsTerminal = True
                        }
            let result = runPureEff . runMockSystem mocks $ detectEnv
            result `shouldBe` CI
