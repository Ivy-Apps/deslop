module Translations.RenderableSpec (spec) where

import Test.Hspec
import Types (Renderable (render))
import Translations.Parser (TransTree (Branch, Leaf, Root))

spec :: Spec
spec = do
  describe "Renderable TransTree" $ do
    it "renders empty Root as {}" $
      render (Root []) `shouldBe` "{}"

    it "renders single Leaf" $
      render (Root [Leaf "k" "v"]) `shouldBe` "{\n  \"k\": \"v\"\n}"

    it "renders multiple Leaves" $
      render (Root [Leaf "a" "1", Leaf "b" "2"])
        `shouldBe` "{\n  \"a\": \"1\",\n  \"b\": \"2\"\n}"

    it "renders nested Branch" $
      render (Root [Branch "nested" [Leaf "x" "y"]])
        `shouldBe` "{\n  \"nested\": {\n    \"x\": \"y\"\n  }\n}"

    it "renders mixed Root with Branch and Leaf" $
      render (Root [Leaf "top" "value", Branch "inner" [Leaf "k" "v"]])
        `shouldBe` "{\n  \"top\": \"value\",\n  \"inner\": {\n    \"k\": \"v\"\n  }\n}"
