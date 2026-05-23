module Monetization.CaptchaSpec (spec) where

import Data.Text qualified as T
import Text.Read (read)
import Doubles.Random (runMockRandom)
import Effectful (runEff, runPureEff)
import Effects.Random (runRandom)
import Hedgehog ((===))
import Monetization.Captcha (Captcha (..), additionCaptcha, randomCaptcha, subtractionCaptcha)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (annotate, assert, evalIO, failure)
import TestUtils (prop)

spec :: Spec
spec = describe "Monetization.Captcha" $ do
    describe "additionCaptcha" $ do
        prop "answer is in [0, 100]" $ do
            captcha <- evalIO . runEff . runRandom $ additionCaptcha
            let ans = read (toString captcha.answer) :: Int
            assert (ans >= 0 && ans <= 100)

        prop "challenge string is consistent with answer" $ do
            captcha <- evalIO . runEff . runRandom $ additionCaptcha
            case T.splitOn "+" (T.dropEnd 2 captcha.challenge) of
                [aStr, bStr] -> do
                    let a = read (toString aStr) :: Int
                        b = read (toString bStr) :: Int
                    captcha.answer === show (a + b)
                _ -> do
                    annotate $ "unexpected challenge format: " <> toString captcha.challenge
                    failure

        it "generates correct captcha for 3 + 5" $ do
            let captcha = runPureEff . runMockRandom [3, 5] $ additionCaptcha
            captcha `shouldBe` Captcha {challenge = "3+5=?", answer = "8"}

        it "generates correct captcha for 0 + 0" $ do
            let captcha = runPureEff . runMockRandom [0, 0] $ additionCaptcha
            captcha `shouldBe` Captcha {challenge = "0+0=?", answer = "0"}

    describe "subtractionCaptcha" $ do
        prop "answer is in [0, 100]" $ do
            captcha <- evalIO . runEff . runRandom $ subtractionCaptcha
            let ans = read (toString captcha.answer) :: Int
            assert (ans >= 0 && ans <= 100)

        prop "challenge string is consistent with answer" $ do
            captcha <- evalIO . runEff . runRandom $ subtractionCaptcha
            case T.splitOn "-" (T.dropEnd 2 captcha.challenge) of
                [aStr, bStr] -> do
                    let a = read (toString aStr) :: Int
                        b = read (toString bStr) :: Int
                    captcha.answer === show (a - b)
                _ -> do
                    annotate $ "unexpected challenge format: " <> toString captcha.challenge
                    failure

        it "generates correct captcha for 7 - 3" $ do
            let captcha = runPureEff . runMockRandom [7, 3] $ subtractionCaptcha
            captcha `shouldBe` Captcha {challenge = "7-3=?", answer = "4"}

        it "generates correct captcha for 5 - 5" $ do
            let captcha = runPureEff . runMockRandom [5, 5] $ subtractionCaptcha
            captcha `shouldBe` Captcha {challenge = "5-5=?", answer = "0"}

    describe "randomCaptcha" $ do
        it "picks addition when index is 0" $ do
            let captcha = runPureEff . runMockRandom [0, 10, 3] $ randomCaptcha
            captcha `shouldBe` Captcha {challenge = "10+3=?", answer = "13"}

        it "picks subtraction when index is 1" $ do
            let captcha = runPureEff . runMockRandom [1, 8, 2] $ randomCaptcha
            captcha `shouldBe` Captcha {challenge = "8-2=?", answer = "6"}
