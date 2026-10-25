{- | What the diagram must hold of any project: nothing points at a node that
is not there, and the arrows it hides as implied are only ever ones between
the project's own modules.
-}
module Deslop.Explore.ModuleGraphPropSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Explore.ModuleGraph (Diagram (..), Edge (..), ModuleNode (..), NodeId (..), diagramOf)
import Deslop.Module (
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    moduleNameUnsafe,
 )
import Deslop.Module qualified as M
import Fixtures.Deslop.Module (mkEdge, mkPathModule, resolvedTo)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Explore.ModuleGraph properties" $ do
    prop "every edge joins two nodes of the diagram" $ do
        modules <- forAll genProject

        let d = diagramOf Nothing Nothing mempty modules
            nodeIds = Set.fromList $ fmap (ModuleAt . (.path)) d.modules <> fmap Package d.externals

        H.assert $ all (\e -> e.source `Set.member` nodeIds && e.target `Set.member` nodeIds) d.edges

    prop "an implied edge only ever joins two of the project's modules" $ do
        modules <- forAll genProject

        let d = diagramOf Nothing Nothing mempty modules

        [e | e <- d.edges, e.implied, isPackage e.target] === []
  where
    isPackage (Package _) = True
    isPackage (ModuleAt _) = False

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
        dirs <- Gen.list (Range.linear 0 3) $ Gen.element ["src", "test", "ui", "a.b"]
        file <- Gen.element ["index.ts", "Page.hs", "util.test.ts", "UtilSpec.hs"]
        pure . T.intercalate "/" $ dirs <> [file]

    genModule pathList path = do
        targets <- Gen.list (Range.linear 0 6) $ genTarget pathList
        pure (mkPathModule path []) {M.edges = zipWith (edgeTo path) [1 ..] targets}

    genTarget pathList =
        (,)
            <$> Gen.frequency
                [ (6, resolvedTo <$> Gen.element pathList)
                , (1, pure . External $ moduleNameUnsafe "react")
                , (1, pure $ resolvedTo "unscanned/x.ts")
                ]
            <*> Gen.element [ImportEdge, ReExportEdge]

    edgeTo path line (target, k) = mkEdge path line "dep" target k
