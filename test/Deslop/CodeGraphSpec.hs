module Deslop.CodeGraphSpec (spec) where

import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (buildModuleGraph, hasPath)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Test.Hspec
import TypeScript.ModuleResolver (moduleIdUnsafe)

-- | Build a minimal AstModule with given id and import targets.
mkModule :: Text -> [Text] -> AstModule
mkModule i targets =
    AstModule
        { id = moduleIdUnsafe i
        , nodes = [ImportNode {target = moduleIdUnsafe t} | t <- targets]
        }

runHasPath :: AstModule -> AstModule -> [AstModule] -> Bool
runHasPath from to modules =
    runPureEff
        . runReader (buildModuleGraph modules)
        $ hasPath from.id to.id

spec :: Spec
spec = describe "Deslop.CodeGraph" $ do
    describe "hasPath" $ do
        it "direct path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
            runHasPath a b [a, b] `shouldBe` True

        it "transitive path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["c"]
                c = mkModule "c" []
            runHasPath a c [a, b, c] `shouldBe` True

        it "no path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
                c = mkModule "c" []
            runHasPath a c [a, b, c] `shouldBe` False

        it "no path in reverse direction" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
            runHasPath b a [a, b] `shouldBe` False

        it "cycle" $ do
            let a = mkModule "a" ["b", "c"]
                b = mkModule "b" ["c"]
                c = mkModule "c" ["a"]
            runHasPath a c [a, b, c] `shouldBe` True
