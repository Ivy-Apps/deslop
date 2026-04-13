module UtilsSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Utils (dropCommonPre)

spec :: Spec
spec = describe "Utils" $ do
    describe "dropCommonPre" $ do
        it "has common prefix" $ do
            let a = "Hello, world!"
            let b = "Hello, Haskell!"
            let (a', b') = dropCommonPre (a, b)
            a' `shouldBe` "world!"
            b' `shouldBe` "Haskell!"

        it "no common prefix" $ do
            let a = "Hello, world!"
            let b = "hi, world!"
            let (a', b') = dropCommonPre (a, b)
            a' `shouldBe` a
            b' `shouldBe` b
