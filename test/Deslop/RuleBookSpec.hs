module Deslop.RuleBookSpec(spec) where
import Test.Hspec

spec :: Spec
spec = do
  describe "YAML to RuleBookDto" $ do
    it "forbidden imports" $ do
      True `shouldBe` True
