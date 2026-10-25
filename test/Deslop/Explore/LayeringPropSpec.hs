{- | What the diagram's simplifications must never lose, whatever the graph:
hiding the implied edges keeps every module reaching what it reached, and
every arrow points down.
-}
module Deslop.Explore.LayeringPropSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Deslop.Explore.Layering (Graph, condense, impliedEdges, layers)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Explore.Layering properties" $ do
    prop "dropping the implied edges keeps everything each vertex reaches" $ do
        g <- forAll genGraph

        let reduced = Map.mapWithKey (\v -> Set.filter (\w -> (v, w) `Set.notMember` impliedEdges g)) g

        closureOf reduced === closureOf g

    prop "every edge leaving a cycle points to a lower layer" $ do
        g <- forAll genGraph

        let rank = layers g
            (componentOf, _) = condense g

        H.assert $
            and
                [ rank Map.! v < rank Map.! w
                | (v, ws) <- Map.toList g
                , w <- Set.toList ws
                , componentOf Map.! v /= componentOf Map.! w
                ]

    prop "only ever marks edges the graph has" $ do
        g <- forAll genGraph

        H.assert $ all (\(v, w) -> w `Set.member` Map.findWithDefault Set.empty v g) (impliedEdges g)

-- | Up to 20 vertices, sparse enough for long paths and dense enough for cycles.
genGraph :: Gen (Graph Int)
genGraph = do
    n <- Gen.int (Range.linear 0 20)
    let vertices = [0 .. n - 1]
    Map.fromList <$> traverse (\v -> (v,) . Set.fromList <$> Gen.list (Range.linear 0 3) (Gen.element vertices)) vertices

-- | Everything each vertex reaches through one edge or more.
closureOf :: (Ord k) => Graph k -> Map k (Set k)
closureOf g = Map.mapWithKey (\v _ -> go Set.empty (Set.toList $ g Map.! v)) g
  where
    go seen [] = seen
    go seen (w : rest)
        | w `Set.member` seen = go seen rest
        | otherwise = go (Set.insert w seen) (Set.toList (g Map.! w) <> rest)
