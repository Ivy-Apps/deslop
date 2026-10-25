module Deslop.Explore.ModuleGraphSpec (spec) where

import Data.Set qualified as Set
import Deslop.Explore.ModuleGraph (
    Diagram (..),
    Edge (..),
    FolderNode (..),
    FolderPath (..),
    ModuleNode (..),
    NodeId (..),
    diagramOf,
 )
import Deslop.Module (
    EdgeKind (..),
    ModuleRole (..),
 )
import Deslop.Module qualified as M
import Fixtures.Deslop.Module (mkBenchAt, mkEdge, mkPathModule, mkTestAt, mkTestOf, resolvedTo)
import Git.SourceLink (Diff (..), GitRef (..))
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Deslop.Explore.ModuleGraph" $ do
    describe "diagramOf" $ do
        it "layers modules from the entry point down to what imports nothing" $ do
            let d = diagramOf Nothing Nothing Nothing [mkPathModule "app/Main.hs" ["src/App.hs"], mkPathModule "src/App.hs" ["src/Util.hs"], mkPathModule "src/Util.hs" []]

            layerOf d <$> ["app/Main.hs", "src/App.hs", "src/Util.hs"] `shouldBe` [Just 0, Just 1, Just 2]

        it "marks the arrow a longer path already implies" $ do
            let d = diagramOf Nothing Nothing Nothing [mkPathModule "Main.hs" ["App.hs", "Util.hs"], mkPathModule "App.hs" ["Util.hs"], mkPathModule "Util.hs" []]

            [(e.source, e.target) | e <- d.edges, e.implied] `shouldBe` [(ModuleAt (rp "Main.hs"), ModuleAt (rp "Util.hs"))]

        it "counts every time a module depends on another, per kind" $ do
            let m =
                    (mkPathModule "a.ts" [])
                        { M.edges =
                            [ mkEdge "a.ts" 1 "./b" (resolvedTo "b.ts") ImportEdge
                            , mkEdge "a.ts" 2 "./b" (resolvedTo "b.ts") ImportEdge
                            , mkEdge "a.ts" 3 "./b" (resolvedTo "b.ts") ReExportEdge
                            ]
                        }

            let d = diagramOf Nothing Nothing Nothing [m, mkPathModule "b.ts" []]

            [(e.kind, e.count) | e <- d.edges] `shouldBe` [(ImportEdge, 2), (ReExportEdge, 1)]

        it "folds a module most of the project imports into a hub, but no test" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        Nothing
                        [ mkPathModule "src/A.hs" ["src/Path.hs"]
                        , mkPathModule "src/B.hs" ["src/Path.hs"]
                        , mkPathModule "src/C.hs" ["src/Path.hs"]
                        , mkPathModule "src/D.hs" []
                        , mkPathModule "src/Path.hs" []
                        , mkTestAt "test/PathSpec.hs" ["src/Path.hs", "test/Helpers.hs"]
                        , mkTestAt "test/Helpers.hs" []
                        ]

            [m.path | m <- d.modules, m.hub] `shouldBe` [rp "src/Path.hs"]

        it "does not call a module a hub with fewer than three importers" $ do
            let d = diagramOf Nothing Nothing Nothing [mkPathModule "A.hs" ["Util.hs"], mkPathModule "B.hs" ["Util.hs"], mkPathModule "Util.hs" []]

            [m.path | m <- d.modules, m.hub] `shouldBe` []

        it "never folds a benchmark into a hub, nor counts its imports" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        Nothing
                        [ mkPathModule "src/A.hs" ["src/Path.hs"]
                        , mkPathModule "src/B.hs" []
                        , mkPathModule "src/Path.hs" []
                        , mkBenchAt "bench/A.hs" ["src/Path.hs", "bench/Clock.hs"]
                        , mkBenchAt "bench/B.hs" ["src/Path.hs", "bench/Clock.hs"]
                        , mkBenchAt "bench/C.hs" ["src/Path.hs", "bench/Clock.hs"]
                        , mkBenchAt "bench/Clock.hs" []
                        ]

            [m.path | m <- d.modules, m.hub] `shouldBe` []

        it "keeps the role each module's frontend gave it" $ do
            let d = diagramOf Nothing Nothing Nothing [mkPathModule "src/a.ts" [], mkTestAt "src/a.check.ts" [], mkBenchAt "perf/a.ts" []]

            [(m.path, m.role) | m <- d.modules]
                `shouldBe` [(rp "perf/a.ts", Benchmark), (rp "src/a.check.ts", Test Nothing), (rp "src/a.ts", Source)]

        it "marks the modules whose files changed" $ do
            let d = diagramOf Nothing Nothing (Just . Diff (GitRef "origin/main") $ Set.fromList [rp "src/b.ts", rp "README.md"]) [mkPathModule "src/a.ts" [], mkPathModule "src/b.ts" []]

            [(m.path, m.changed) | m <- d.modules] `shouldBe` [(rp "src/a.ts", False), (rp "src/b.ts", True)]

        it "lists as a module's tests those its frontend named it the subject of" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        Nothing
                        [ mkPathModule "src/Deslop/Diagram.hs" []
                        , mkPathModule "src/Other/Diagram.hs" []
                        , mkTestOf "test/Deslop/DiagramSpec.hs" "src/Deslop/Diagram.hs"
                        , mkTestOf "test/Deslop/DiagramPropSpec.hs" "src/Deslop/Diagram.hs"
                        , mkTestAt "test/Doubles/CLI.hs" []
                        ]

            [(m.path, m.testedBy) | m <- d.modules, Source <- [m.role]]
                `shouldBe` [ (rp "src/Deslop/Diagram.hs", [rp "test/Deslop/DiagramPropSpec.hs", rp "test/Deslop/DiagramSpec.hs"])
                           , (rp "src/Other/Diagram.hs", [])
                           ]

        it "lists no test whose subject is not drawn" $ do
            let d = diagramOf Nothing Nothing Nothing [mkPathModule "src/a.ts" [], mkTestOf "src/b.test.ts" "src/b.ts"]

            [(m.path, m.testedBy) | m <- d.modules] `shouldBe` [(rp "src/a.ts", []), (rp "src/b.test.ts", [])]

        it "names every folder above a module, each under its parent" $ do
            let d = diagramOf Nothing Nothing Nothing [mkPathModule "src/ui/Button.tsx" [], mkPathModule "main.ts" []]

            d.folders
                `shouldBe` [ FolderNode {path = FolderPath ("src" :| []), parent = Nothing}
                           , FolderNode {path = FolderPath ("src" :| ["ui"]), parent = Just (FolderPath ("src" :| []))}
                           ]
  where
    layerOf d p = viaNonEmpty head [m.layer | m <- d.modules, m.path == rp p]
