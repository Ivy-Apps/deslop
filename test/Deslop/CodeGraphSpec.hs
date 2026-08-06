module Deslop.CodeGraphSpec (spec) where

import Data.Map.Strict qualified as Map
import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleCycle (..), buildModuleGraph, findCycles, findKnownPath, hasPath, moduleExists, reachableFrom)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Hedgehog (Gen, PropertyT, footnote, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (mkModule, prop)
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)

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

runModuleExists :: Text -> [AstModule] -> Bool
runModuleExists mid modules =
    runPureEff
        . runReader (buildModuleGraph modules)
        $ moduleExists (moduleIdUnsafe mid)

runFindKnownPath :: AstModule -> AstModule -> [AstModule] -> [Text]
runFindKnownPath from to modules =
    map (.text)
        . toList
        . runPureEff
        . runReader (buildModuleGraph modules)
        $ findKnownPath from.id to.id

-- | Runs findCycles and reduces each cycle to its module ids, in walk order.
runFindCycles :: [AstModule] -> [[Text]]
runFindCycles modules =
    map (map (.id.text) . toList . (.modules))
        . runPureEff
        . runReader (buildModuleGraph modules)
        $ findCycles

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

    describe "moduleExists" $ do
        it "returns True for an internal module in the graph" $ do
            let a = mkModule "a" []
            runModuleExists "a" [a] `shouldBe` True

        it "returns False for a module not in the graph" $ do
            let a = mkModule "a" []
            runModuleExists "unknown" [a] `shouldBe` False

        it "returns False for an empty graph" $ do
            runModuleExists "a" [] `shouldBe` False

        it "returns True for an external module referenced as an import target" $ do
            -- "b" is never parsed but referenced by "a", so it exists as ExternalModule
            let a = mkModule "a" ["b"]
            runModuleExists "b" [a] `shouldBe` True

        it "returns False when a sibling module exists but not the queried one" $ do
            let a = mkModule "a" []
                b = mkModule "b" []
            runModuleExists "c" [a, b] `shouldBe` False

    describe "findKnownPath" $ do
        it "returns single-element path for self" $ do
            let a = mkModule "a" []
            runFindKnownPath a a [a] `shouldBe` ["a"]

        it "returns direct path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" []
            runFindKnownPath a b [a, b] `shouldBe` ["a", "b"]

        it "returns shortest path" $ do
            let a = mkModule "a" ["b", "c"]
                b = mkModule "b" ["c"]
                c = mkModule "c" []
            runFindKnownPath a c [a, b, c] `shouldBe` ["a", "c"]

        it "returns multi-hop path" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["c"]
                c = mkModule "c" []
            runFindKnownPath a c [a, b, c] `shouldBe` ["a", "b", "c"]

    describe "findCycles" $ do
        it "finds a two-module cycle" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["a"]
            runFindCycles [a, b] `shouldBe` [["a", "b"]]

        it "finds a three-module cycle" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["c"]
                c = mkModule "c" ["a"]
            runFindCycles [a, b, c] `shouldBe` [["a", "b", "c"]]

        it "finds a module importing itself" $ do
            let a = mkModule "a" ["a"]
            runFindCycles [a] `shouldBe` [["a"]]

        it "returns no cycles for an acyclic graph" $ do
            let a = mkModule "a" ["b", "c"]
                b = mkModule "b" ["c"]
                c = mkModule "c" []
            runFindCycles [a, b, c] `shouldBe` []

        it "returns no cycles for an empty graph" $ do
            runFindCycles [] `shouldBe` []

        it "does not treat an external module as a cycle" $ do
            let a = mkModule "a" ["react"]
            runFindCycles [a] `shouldBe` []

        it "reports each disjoint cycle separately" $ do
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["a"]
                x = mkModule "x" ["y"]
                y = mkModule "y" ["x"]
            sort (runFindCycles [a, b, x, y]) `shouldBe` [["a", "b"], ["x", "y"]]

        it "starts the cycle at the lexicographically smallest module" $ do
            let z = mkModule "z" ["m"]
                m = mkModule "m" ["a"]
                a = mkModule "a" ["z"]
            runFindCycles [z, m, a] `shouldBe` [["a", "z", "m"]]

        it "reports the shortest loop through the start, breaking ties in module order" $ do
            -- both a → b → a and a → c → a are shortest; b wins on module order
            let a = mkModule "a" ["c", "b"]
                b = mkModule "b" ["a"]
                c = mkModule "c" ["a"]
            runFindCycles [a, b, c] `shouldBe` [["a", "b"]]

        it "reports one cycle for a component holding several loops" $ do
            -- a → b → a and b → c → b share module b, so they are one component
            let a = mkModule "a" ["b"]
                b = mkModule "b" ["a", "c"]
                c = mkModule "c" ["b"]
            runFindCycles [a, b, c] `shouldBe` [["a", "b"]]

        prop "reports no cycles for an acyclic graph" $ do
            modules <- forAll genDag
            runFindCycles modules === []

        prop "reports an isolated cycle exactly once, starting at its smallest module" $ do
            (modules, chain) <- forAll genGraphWithIsolatedCycle
            case runFindCycles modules of
                [reported] -> do
                    sort reported === sort chain
                    viaNonEmpty head reported === viaNonEmpty head (sort chain)
                other -> do
                    footnote $ "expected exactly one cycle, got: " <> show other
                    length other === 1

        prop "reports only real cycles" $ do
            modules <- forAll genArbitraryGraph
            traverse_ (assertIsCycle modules) (runFindCycles modules)

        prop "reports the same cycles regardless of input order" $ do
            modules <- forAll genArbitraryGraph
            shuffled <- forAll (Gen.shuffle modules)
            runFindCycles modules === runFindCycles shuffled

{- | Asserts that a reported cycle is a genuine closed walk: distinct modules,
every consecutive hop a real import, the last module importing the first, and the
smallest module first.
-}
assertIsCycle :: [AstModule] -> [Text] -> PropertyT IO ()
assertIsCycle modules reported = do
    footnote $ "cycle: " <> show reported
    ordNub reported === reported
    viaNonEmpty head reported === viaNonEmpty head (sort reported)
    traverse_ assertImports hops
  where
    imports = Map.fromList [(m.id.text, map (.target.text) m.nodes) | m <- modules]
    hops = zip reported (drop 1 reported <> take 1 reported)
    assertImports (from, to) =
        (from, to `elem` Map.findWithDefault [] from imports) === (from, True)

-- | Names are prefixed per group so that generated graphs cannot collide.
moduleName :: Text -> Int -> Text
moduleName prefix i = prefix <> show i

{- | A graph that is acyclic by construction: module i may only import modules
after it. The result is shuffled so no consumer can rely on topological input.
-}
genDag :: Gen [AstModule]
genDag = do
    n <- Gen.int (Range.linear 0 12)
    let names = map (moduleName "m") [0 .. n - 1]
    imports <- traverse (\i -> Gen.subsequence (drop (i + 1) names)) [0 .. n - 1]
    Gen.shuffle (zipWith mkModule names imports)

-- | A graph wired at random, which may contain any number of cycles or none.
genArbitraryGraph :: Gen [AstModule]
genArbitraryGraph = do
    n <- Gen.int (Range.linear 1 10)
    let names = map (moduleName "m") [0 .. n - 1]
    imports <- traverse (const . Gen.subsequence $ names) names
    Gen.shuffle (zipWith mkModule names imports)

{- | A DAG plus a disjoint chain that loops back on itself. The two share no
modules, so the chain is the graph's only cycle. Returns the chain's modules.
-}
genGraphWithIsolatedCycle :: Gen ([AstModule], [Text])
genGraphWithIsolatedCycle = do
    dag <- genDag
    k <- Gen.int (Range.linear 2 6)
    let chain = map (moduleName "c") [0 .. k - 1]
        nextInChain = drop 1 chain <> take 1 chain
        chainModules = zipWith (\name next -> mkModule name [next]) chain nextInChain
    modules <- Gen.shuffle (dag <> chainModules)
    pure (modules, chain)
