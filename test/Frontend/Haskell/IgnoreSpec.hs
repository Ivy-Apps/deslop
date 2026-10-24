module Frontend.Haskell.IgnoreSpec (spec) where

import FileSystem.Iterator (Entry (..))
import Frontend.Haskell.Ignore (hsEcosystemIgnored)
import Test.Hspec
import TestUtils (ap)

ignored :: Text -> Bool
ignored path = hsEcosystemIgnored Entry {path = ap path, isDir = True}

spec :: Spec
spec = describe "Frontend.Haskell.Ignore" $ do
    it "ignores what cabal and stack build into" $
        fmap ignored ["/p/dist-newstyle", "/p/dist", "/p/.stack-work"] `shouldBe` [True, True, True]

    it "ignores tool caches" $
        fmap ignored ["/p/.hie", "/p/.hpc"] `shouldBe` [True, True]

    it "matches the name exactly" $
        fmap ignored ["/p/distribution", "/p/src/Dist"] `shouldBe` [False, False]
