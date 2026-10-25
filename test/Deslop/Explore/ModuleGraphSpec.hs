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
    EdgeTarget (..),
    Module (..),
    ModuleRole (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Deslop.Module qualified as M
import Fixtures.Deslop.Module (mkEdge, mkModuleAt)
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Deslop.Explore.ModuleGraph" $ do
    describe "diagramOf" $ do
        it "layers modules from the entry point down to what imports nothing" $ do
            let d = diagramOf Nothing Nothing mempty [at "app/Main.hs" ["src/App.hs"], at "src/App.hs" ["src/Util.hs"], at "src/Util.hs" []]

            layerOf d <$> ["app/Main.hs", "src/App.hs", "src/Util.hs"] `shouldBe` [Just 0, Just 1, Just 2]

        it "marks the arrow a longer path already implies" $ do
            let d = diagramOf Nothing Nothing mempty [at "Main.hs" ["App.hs", "Util.hs"], at "App.hs" ["Util.hs"], at "Util.hs" []]

            [(e.source, e.target) | e <- d.edges, e.implied] `shouldBe` [(ModuleAt (rp "Main.hs"), ModuleAt (rp "Util.hs"))]

        it "counts every time a module depends on another, per kind" $ do
            let m =
                    (at "a.ts" [])
                        { M.edges =
                            [ mkEdge "a.ts" 1 "./b" (internal "b.ts") ImportEdge
                            , mkEdge "a.ts" 2 "./b" (internal "b.ts") ImportEdge
                            , mkEdge "a.ts" 3 "./b" (internal "b.ts") ReExportEdge
                            ]
                        }

            let d = diagramOf Nothing Nothing mempty [m, at "b.ts" []]

            [(e.kind, e.count) | e <- d.edges] `shouldBe` [(ImportEdge, 2), (ReExportEdge, 1)]

        it "folds a module most of the project imports into a hub, but no test" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        mempty
                        [ at "src/A.hs" ["src/Path.hs"]
                        , at "src/B.hs" ["src/Path.hs"]
                        , at "src/C.hs" ["src/Path.hs"]
                        , at "src/D.hs" []
                        , at "src/Path.hs" []
                        , testAt "test/PathSpec.hs" ["src/Path.hs", "test/Helpers.hs"]
                        , testAt "test/Helpers.hs" []
                        ]

            [m.path | m <- d.modules, m.hub] `shouldBe` [rp "src/Path.hs"]

        it "does not call a module a hub with fewer than three importers" $ do
            let d = diagramOf Nothing Nothing mempty [at "A.hs" ["Util.hs"], at "B.hs" ["Util.hs"], at "Util.hs" []]

            [m.path | m <- d.modules, m.hub] `shouldBe` []

        it "never folds a benchmark into a hub, nor counts its imports" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        mempty
                        [ at "src/A.hs" ["src/Path.hs"]
                        , at "src/B.hs" []
                        , at "src/Path.hs" []
                        , benchAt "bench/A.hs" ["src/Path.hs", "bench/Clock.hs"]
                        , benchAt "bench/B.hs" ["src/Path.hs", "bench/Clock.hs"]
                        , benchAt "bench/C.hs" ["src/Path.hs", "bench/Clock.hs"]
                        , benchAt "bench/Clock.hs" []
                        ]

            [m.path | m <- d.modules, m.hub] `shouldBe` []

        it "keeps the role each module's frontend gave it" $ do
            let d = diagramOf Nothing Nothing mempty [at "src/a.ts" [], testAt "src/a.check.ts" [], benchAt "perf/a.ts" []]

            [(m.path, m.role) | m <- d.modules]
                `shouldBe` [(rp "perf/a.ts", Benchmark), (rp "src/a.check.ts", Test Nothing), (rp "src/a.ts", Source)]

        it "marks the modules whose files changed" $ do
            let d = diagramOf Nothing Nothing (Set.fromList [rp "src/b.ts", rp "README.md"]) [at "src/a.ts" [], at "src/b.ts" []]

            [(m.path, m.changed) | m <- d.modules] `shouldBe` [(rp "src/a.ts", False), (rp "src/b.ts", True)]

        it "lists as a module's tests those its frontend named it the subject of" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        mempty
                        [ at "src/Deslop/Diagram.hs" []
                        , at "src/Other/Diagram.hs" []
                        , testOf "test/Deslop/DiagramSpec.hs" "src/Deslop/Diagram.hs"
                        , testOf "test/Deslop/DiagramPropSpec.hs" "src/Deslop/Diagram.hs"
                        , testAt "test/Doubles/CLI.hs" []
                        ]

            [(m.path, m.testedBy) | m <- d.modules, Source <- [m.role]]
                `shouldBe` [ (rp "src/Deslop/Diagram.hs", [rp "test/Deslop/DiagramPropSpec.hs", rp "test/Deslop/DiagramSpec.hs"])
                           , (rp "src/Other/Diagram.hs", [])
                           ]

        it "lists no test whose subject is not drawn" $ do
            let d = diagramOf Nothing Nothing mempty [at "src/a.ts" [], testOf "src/b.test.ts" "src/b.ts"]

            [(m.path, m.testedBy) | m <- d.modules] `shouldBe` [(rp "src/a.ts", []), (rp "src/b.test.ts", [])]

        it "names every folder above a module, each under its parent" $ do
            let d = diagramOf Nothing Nothing mempty [at "src/ui/Button.tsx" [], at "main.ts" []]

            d.folders
                `shouldBe` [ FolderNode {path = FolderPath ("src" :| []), parent = Nothing}
                           , FolderNode {path = FolderPath ("src" :| ["ui"]), parent = Just (FolderPath ("src" :| []))}
                           ]
  where
    layerOf d p = viaNonEmpty head [m.layer | m <- d.modules, m.path == rp p]

-- | A module at a path, named and identified by it, importing modules by their paths.
at :: Text -> [Text] -> Module
at path = mkModuleAt path path

testAt :: Text -> [Text] -> Module
testAt path deps = (at path deps) {M.role = Test Nothing}

-- | A test its frontend named the module at this path the subject of.
testOf :: Text -> Text -> Module
testOf path subject = (at path []) {M.role = Test (Just $ moduleIdUnsafe subject)}

benchAt :: Text -> [Text] -> Module
benchAt path deps = (at path deps) {M.role = Benchmark}

internal :: Text -> EdgeTarget
internal p = Resolved (moduleIdUnsafe p) (moduleNameUnsafe p :| [])
