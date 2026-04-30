module Deslop.CodeGraphSpec (spec) where

import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (buildModuleGraph, findPath, hasPath, reachableFrom)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Test.Hspec
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)

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

runReachableFrom :: AstModule -> [AstModule] -> [Text]
runReachableFrom from modules =
    sort
        . map (.text)
        . runPureEff
        . runReader (buildModuleGraph modules)
        $ reachableFrom from.id

runFindPath :: AstModule -> AstModule -> [AstModule] -> Maybe [Text]
runFindPath from to modules =
    fmap (map (.text) . toList)
        . runPureEff
        . runReader (buildModuleGraph modules)
        $ findPath from.id to.id

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

    describe "reachableFrom" $ do
        it "returns only self when module has no imports" $ do
            let a = mkModule "a" []
            runReachableFrom a [a] `shouldBe` ["a"]

        it "returns self and direct imports" $ do
            let a = mkModule "a" ["b", "c"]
                b = mkModule "b" []
                c = mkModule "c" []
            runReachableFrom a [a, b, c] `shouldBe` ["a", "b", "c"]

        it "returns full transitive closure" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["c"]
                c = mkModule "c" []
            runReachableFrom a [a, b, c] `shouldBe` ["a", "b", "c"]

        it "does not include unreachable modules" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
                c = mkModule "c" []
            runReachableFrom a [a, b, c] `shouldBe` ["a", "b"]

        it "returns empty list when module is not in the graph" $ do
            let a = mkModule "a" []
                unknown = mkModule "unknown" []
            runReachableFrom unknown [a] `shouldBe` []

        it "handles cycles without infinite loop" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["c"]
                c = mkModule "c" ["a"]
            runReachableFrom a [a, b, c] `shouldBe` ["a", "b", "c"]

    describe "findPath" $ do
        it "returns Nothing when target is unreachable" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
                c = mkModule "c" []
            runFindPath a c [a, b, c] `shouldBe` Nothing

        it "returns Nothing when source is not in the graph" $ do
            let a = mkModule "a" []
                unknown = mkModule "unknown" []
            runFindPath unknown a [a] `shouldBe` Nothing

        it "returns single-element path for self" $ do
            let a = mkModule "a" []
            runFindPath a a [a] `shouldBe` Just ["a"]

        it "returns direct path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
            runFindPath a b [a, b] `shouldBe` Just ["a", "b"]

        it "returns multi-hop path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["c"]
                c = mkModule "c" []
            runFindPath a c [a, b, c] `shouldBe` Just ["a", "b", "c"]

        it "returns Nothing in reverse direction" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
            runFindPath b a [a, b] `shouldBe` Nothing
