module Deslop.Explore.LayeringSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Deslop.Explore.Layering (Graph, impliedEdges, layers)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Explore.Layering" $ do
    describe "layers" $ do
        it "puts the entry point on top and what imports nothing at the bottom" $
            layers (graph [("main", ["app", "util"]), ("app", ["domain"]), ("domain", ["util"])])
                `shouldBe` Map.fromList [("main", 0), ("app", 1), ("domain", 2), ("util", 3)]

        it "ranks a module just above the deepest thing it imports" $
            layers (graph [("main", ["util"]), ("app", ["util"])])
                `shouldBe` Map.fromList [("main", 0), ("app", 0), ("util", 1)]

        it "gives the members of a cycle one layer, above what the cycle imports" $
            layers (graph [("a", ["b"]), ("b", ["a", "c"])])
                `shouldBe` Map.fromList [("a", 0), ("b", 0), ("c", 1)]

        it "puts a module with no edges at all on the bottom layer" $
            layers (graph [("main", ["util"]), ("loner", [])])
                `shouldBe` Map.fromList [("main", 0), ("util", 1), ("loner", 1)]

        it "ranks nothing in an empty graph" $
            layers (graph []) `shouldBe` Map.empty

    describe "impliedEdges" $ do
        it "marks the shortcut a longer path already takes" $
            impliedEdges (graph [("main", ["app", "util"]), ("app", ["util"])])
                `shouldBe` Set.singleton ("main", "util")

        it "keeps every edge of a graph with no shortcut" $
            impliedEdges (graph [("main", ["app", "lib"]), ("app", ["util"]), ("lib", ["util"])])
                `shouldBe` Set.empty

        it "never marks an edge inside a cycle" $
            impliedEdges (graph [("a", ["b", "c"]), ("b", ["c"]), ("c", ["a"])])
                `shouldBe` Set.empty

        it "marks a shortcut that goes around a cycle" $
            impliedEdges (graph [("main", ["a", "util"]), ("a", ["b"]), ("b", ["a", "util"])])
                `shouldBe` Set.singleton ("main", "util")

-- | Every vertex named, as a key, whether or not anything points at it.
graph :: [(Text, [Text])] -> Graph Text
graph adjacency =
    Map.fromListWith (<>) $
        [(v, Set.fromList ws) | (v, ws) <- adjacency] <> [(w, Set.empty) | (_, ws) <- adjacency, w <- ws]
