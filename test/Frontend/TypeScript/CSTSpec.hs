module Frontend.TypeScript.CSTSpec (spec) where

import Frontend.TypeScript.CST (TsNode (..), onLines, specifierOf, withSpecifier)
import Generators.Frontend.TypeScript.CST (genTsProgram)
import Hedgehog (forAll, (===))
import Renderable (Renderable (render))
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Frontend.TypeScript.CST" $ do
    describe "specifierOf" $ do
        it "names the module an import or a re-export depends on, and nothing for source" $
            fmap specifierOf [Import "import x from '" "@/lib" "';", ReExport "export * from '" "./a" "';", Source "x;"]
                `shouldBe` [Just "@/lib", Just "./a", Nothing]

    describe "withSpecifier" $ do
        it "rewrites only the specifier, keeping every other byte of the statement" $
            render (withSpecifier "@/lib/a" (Import "import { a } from \"" "../lib/a" "\";\n"))
                `shouldBe` "import { a } from \"@/lib/a\";\n"

        it "leaves source alone" $
            withSpecifier "@/lib/a" (Source "const a = 1;") `shouldBe` Source "const a = 1;"

        prop "names the new specifier, whatever the statement" $ do
            nodes <- forAll genTsProgram
            fmap (specifierOf . withSpecifier "@/x") nodes === fmap (("@/x" <$) . specifierOf) nodes

    describe "onLines" $ do
        it "numbers each node by the line its first character sits on" $
            fmap fst (onLines [Source "a\nb\n", Import "import '" "./c" "';\n", Source "\n", Source "d"])
                `shouldBe` [1, 3, 4, 5]

        it "numbers nothing in an empty file" $
            onLines [] `shouldBe` []
