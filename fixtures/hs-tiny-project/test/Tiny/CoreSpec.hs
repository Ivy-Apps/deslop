module Tiny.CoreSpec (spec) where

import Test.Hspec
import Tiny.Core

spec :: Spec
spec = it "greets" $ greet (Name "x") `shouldBe` "Hello, x"
