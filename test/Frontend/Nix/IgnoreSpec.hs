module Frontend.Nix.IgnoreSpec (spec) where

import FileSystem.Iterator (Entry (..))
import Frontend.Nix.Ignore (nixEcosystemIgnored)
import Test.Hspec
import TestUtils (ap)

ignored :: Text -> Bool
ignored path = nixEcosystemIgnored Entry {path = ap path, isDir = True}

spec :: Spec
spec = describe "Frontend.Nix.Ignore" $ do
    it "ignores the links nix build leaves" $
        fmap ignored ["/p/result", "/p/result-dev", "/p/hosts/result-man"] `shouldBe` [True, True, True]

    it "matches the name, not a prefix of it" $
        fmap ignored ["/p/results", "/p/resultant", "/p/my-result"] `shouldBe` [False, False, False]
