{- | What the diagram must preserve of the project whatever its shape: every
dependency it draws is accounted for, and nothing points at a box that is not
there.
-}
module Deslop.DiagramPropSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Diagram (
    Arrow (..),
    Board (..),
    Diagram (..),
    Graph (..),
    Layout (..),
    diagramOf,
    layoutOf,
    placeOf,
 )
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Fixtures.Deslop.Module (mkEdge, mkModuleAt)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Diagram properties" $ do
    prop "the overview's counts add up to every dependency crossing between its boxes" $ do
        modules <- forAll genProject

        let d = diagramOf modules

        sum (Map.elems d.overview.arrows) === length (crossingEdges modules)

    prop "every arrow of the overview joins two of its boxes" $ do
        modules <- forAll genProject

        let g = (diagramOf modules).overview

        H.assert $ all (joins g) (Map.keys g.arrows)

    prop "every arrow of a board joins two of its boxes" $ do
        modules <- forAll genProject

        let boards = (diagramOf modules).boards

        H.assert $ all (\b -> all (joins b.graph) (Map.keys b.graph.arrows)) boards
  where
    joins g a = a.from `Set.member` g.nodes && a.to `Set.member` g.nodes

{- | Every edge between two parsed modules whose places on the overview differ,
judged edge by edge rather than by the diagram's own tally.
-}
crossingEdges :: [Module] -> [DependencyEdge]
crossingEdges modules =
    [ e
    | m <- modules
    , e <- m.edges
    , Resolved target _ <- [e.target]
    , Just from <- [placeFor m.id]
    , Just to <- [placeFor target]
    , from /= to
    ]
  where
    placeFor mid = placeOf <$> Map.lookup mid (layoutOf modules).spots

{- | Modules at distinct paths of mixed depth, depending on one another, on
packages, and on modules nobody parsed - through imports and re-exports alike.
-}
genProject :: Gen [Module]
genProject = do
    paths <- Gen.set (Range.linear 1 25) genPath
    let pathList = Set.toList paths
    traverse (genModule pathList) pathList
  where
    genPath = do
        dirs <- Gen.list (Range.linear 0 4) $ Gen.element ["src", "app", "lib", "[id]", "ui", "a.b"]
        file <- Gen.element ["index.ts", "page.tsx", "util.ts", "x.stories.tsx"]
        pure . T.intercalate "/" $ dirs <> [file]

    genModule pathList path = do
        targets <- Gen.list (Range.linear 0 6) $ genTarget pathList
        pure (mkModuleAt path path []) {edges = zipWith (edgeTo path) [1 ..] targets}

    genTarget pathList =
        (,)
            <$> Gen.frequency
                [ (6, internal <$> Gen.element pathList)
                , (1, pure . External $ moduleNameUnsafe "react")
                , (1, pure $ internal "unscanned/x.ts")
                ]
            <*> genKind

    internal p = Resolved (moduleIdUnsafe p) (moduleNameUnsafe p :| [])

    genKind = Gen.element [ImportEdge, ReExportEdge]

    edgeTo path line (target, k) = mkEdge path line "dep" target k
