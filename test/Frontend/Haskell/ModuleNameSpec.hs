module Frontend.Haskell.ModuleNameSpec (spec) where

import Deslop.Module (ModuleName (..))
import Frontend.Haskell.ModuleName (dotted, fromDotted, fromSegments, hsModuleName, mainModule, slashed)
import Test.Hspec

spec :: Spec
spec = describe "Frontend.Haskell.ModuleName" $ do
    describe "reading a dotted name" $ do
        it "splits it at its dots" $
            fmap dotted (fromDotted "Data.List.NonEmpty") `shouldBe` Just "Data.List.NonEmpty"

        it "refuses a segment that is not a constructor name" $
            fromDotted "Data.list" `shouldBe` Nothing

        it "refuses an empty segment" $
            fromDotted "Data..List" `shouldBe` Nothing

        it "refuses nothing at all" $
            fromSegments [] `shouldBe` Nothing

        it "keeps primes and underscores, which a module name may hold" $
            fmap dotted (fromDotted "My_Module'") `shouldBe` Just "My_Module'"

    describe "as a Module Name" $ do
        it "is spelled with slashes, as in every language" $
            fmap ((.text) . slashed) (fromDotted "Data.Text") `shouldBe` Just "Data/Text"

        it "keeps a name it never validated, such as a path's" $
            (slashed (hsModuleName ("scripts" :| ["build-all"]))).text `shouldBe` "scripts/build-all"

    it "calls a file without a header Main" $
        dotted mainModule `shouldBe` "Main"
