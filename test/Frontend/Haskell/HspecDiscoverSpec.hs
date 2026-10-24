module Frontend.Haskell.HspecDiscoverSpec (spec) where

import Frontend.Haskell.HspecDiscover (DiscoveredSpec (..), discoveredSpecs)
import Frontend.Haskell.ModuleName (dotted)
import Test.Hspec
import TestUtils (ap)

discovered :: Text -> [Text] -> [Text]
discovered driver = fmap (dotted . (.name)) . discoveredSpecs (ap driver) . fmap ap

spec :: Spec
spec = describe "Frontend.Haskell.HspecDiscover" $ do
    it "imports every spec below the driver's directory, named from it" $
        discovered "/p/test/Spec.hs" ["/p/test/FooSpec.hs", "/p/test/Deslop/ModuleSpec.hs"]
            `shouldBe` ["FooSpec", "Deslop.ModuleSpec"]

    it "does not import the driver itself" $
        discovered "/p/test/MainSpec.hs" ["/p/test/MainSpec.hs", "/p/test/ASpec.hs"] `shouldBe` ["ASpec"]

    it "does not import Spec.hs, whose name before Spec is empty" $
        discovered "/p/test/Main.hs" ["/p/test/Spec.hs", "/p/test/Deslop/Spec.hs"] `shouldBe` []

    it "does not import a file whose name does not end in Spec" $
        discovered "/p/test/Spec.hs" ["/p/test/TestUtils.hs", "/p/test/Fixtures/Specs.hs"] `shouldBe` []

    it "does not import a spec outside the driver's directory" $
        discovered "/p/test/unit/Spec.hs" ["/p/test/ASpec.hs", "/p/src/BSpec.hs"] `shouldBe` []

    it "does not import a spec whose path is no module name" $
        discovered "/p/test/Spec.hs" ["/p/test/lower/ASpec.hs", "/p/test/my-dir/BSpec.hs"] `shouldBe` []
