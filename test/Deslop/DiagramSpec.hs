module Deslop.DiagramSpec (spec) where

import Data.Aeson (Value, decode, encode, object, (.=))
import Deslop.Diagram (
    Diagram (..),
    Edge (..),
    FolderNode (..),
    FolderPath (..),
    ModuleNode (..),
    NodeId (..),
    Role (..),
    diagramOf,
 )
import Deslop.Module (
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Deslop.Module qualified as M
import Fixtures.Deslop.Module (mkEdge, mkModuleAt)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Diagram" $ do
    describe "diagramOf" $ do
        it "layers modules from the entry point down to what imports nothing" $ do
            let d = diagramOf [at "app/Main.hs" ["src/App.hs"], at "src/App.hs" ["src/Util.hs"], at "src/Util.hs" []]

            layerOf d <$> ["app/Main.hs", "src/App.hs", "src/Util.hs"] `shouldBe` [Just 0, Just 1, Just 2]

        it "marks the arrow a longer path already implies" $ do
            let d = diagramOf [at "Main.hs" ["App.hs", "Util.hs"], at "App.hs" ["Util.hs"], at "Util.hs" []]

            [(e.source, e.target) | e <- d.edges, e.implied] `shouldBe` [(ModuleAt "Main.hs", ModuleAt "Util.hs")]

        it "counts every time a module depends on another, per kind" $ do
            let m =
                    (at "a.ts" [])
                        { M.edges =
                            [ mkEdge "a.ts" 1 "./b" (internal "b.ts") ImportEdge
                            , mkEdge "a.ts" 2 "./b" (internal "b.ts") ImportEdge
                            , mkEdge "a.ts" 3 "./b" (internal "b.ts") ReExportEdge
                            ]
                        }

            let d = diagramOf [m, at "b.ts" []]

            [(e.kind, e.count) | e <- d.edges] `shouldBe` [(ImportEdge, 2), (ReExportEdge, 1)]

        it "folds a module most of the project imports into a hub, but no test" $ do
            let d =
                    diagramOf
                        [ at "src/A.hs" ["src/Path.hs"]
                        , at "src/B.hs" ["src/Path.hs"]
                        , at "src/C.hs" ["src/Path.hs"]
                        , at "src/D.hs" []
                        , at "src/Path.hs" []
                        , at "test/PathSpec.hs" ["src/Path.hs", "test/Helpers.hs"]
                        , at "test/Helpers.hs" []
                        ]

            [m.path | m <- d.modules, m.hub] `shouldBe` ["src/Path.hs"]

        it "does not call a module a hub with fewer than three importers" $ do
            let d = diagramOf [at "A.hs" ["Util.hs"], at "B.hs" ["Util.hs"], at "Util.hs" []]

            [m.path | m <- d.modules, m.hub] `shouldBe` []

        it "tells tests from source by their folder or their name" $ do
            let d =
                    diagramOf
                        [ at "src/a/b.ts" []
                        , at "src/a/b.test.ts" []
                        , at "src/a/__tests__/c.ts" []
                        , at "test/Doubles/CLI.hs" []
                        , at "testing/Tools.hs" []
                        ]

            [(m.path, m.role) | m <- d.modules]
                `shouldBe` [ ("src/a/__tests__/c.ts", Test)
                           , ("src/a/b.test.ts", Test)
                           , ("src/a/b.ts", Source)
                           , ("test/Doubles/CLI.hs", Test)
                           , ("testing/Tools.hs", Source)
                           ]

        it "links a module to the specs named after it" $ do
            let d =
                    diagramOf
                        [ at "src/Deslop/Diagram.hs" []
                        , at "src/Other/Diagram.hs" []
                        , at "src/Deslop.hs" []
                        , at "test/Deslop/DiagramSpec.hs" []
                        , at "test/Deslop/DiagramPropSpec.hs" []
                        , at "test/DeslopSpec.hs" []
                        , at "test/Doubles/CLI.hs" []
                        ]

            [(m.path, m.testedBy) | m <- d.modules, m.role == Source]
                `shouldBe` [ ("src/Deslop/Diagram.hs", ["test/Deslop/DiagramPropSpec.hs", "test/Deslop/DiagramSpec.hs"])
                           , ("src/Deslop.hs", ["test/DeslopSpec.hs"])
                           , ("src/Other/Diagram.hs", [])
                           ]

        it "names every folder above a module, each under its parent" $ do
            let d = diagramOf [at "src/ui/Button.tsx" [], at "main.ts" []]

            d.folders
                `shouldBe` [ FolderNode {path = FolderPath ("src" :| []), parent = Nothing}
                           , FolderNode {path = FolderPath ("src" :| ["ui"]), parent = Just (FolderPath ("src" :| []))}
                           ]

    describe "JSON" $ do
        it "writes nodes and edges in the deslop-graph/1 schema" $ do
            let m = (at "src/a.ts" []) {M.edges = [mkEdge "src/a.ts" 1 "react" (External $ moduleNameUnsafe "react") ImportEdge]}

            decode (encode $ diagramOf [m])
                `shouldBe` Just
                    ( object
                        [ "schema" .= ("deslop-graph/1" :: Text)
                        , "nodes"
                            .= [ object
                                    [ "id" .= ("src/" :: Text)
                                    , "kind" .= ("folder" :: Text)
                                    , "label" .= ("src" :: Text)
                                    , "path" .= ("src" :: Text)
                                    , "parent" .= (Nothing :: Maybe Text)
                                    ]
                               , object
                                    [ "id" .= ("src/a.ts" :: Text)
                                    , "kind" .= ("module" :: Text)
                                    , "name" .= ("src/a.ts" :: Text)
                                    , "aliases" .= ([] :: [Text])
                                    , "path" .= ("src/a.ts" :: Text)
                                    , "parent" .= ("src/" :: Text)
                                    , "layer" .= (0 :: Int)
                                    , "tags" .= ([] :: [Text])
                                    , "testedBy" .= ([] :: [Text])
                                    ]
                               , object
                                    [ "id" .= ("external:react" :: Text)
                                    , "kind" .= ("external" :: Text)
                                    , "name" .= ("react" :: Text)
                                    , "tags" .= ["external" :: Text]
                                    ]
                               ]
                        , "edges"
                            .= [ object
                                    [ "source" .= ("src/a.ts" :: Text)
                                    , "target" .= ("external:react" :: Text)
                                    , "kind" .= ("import" :: Text)
                                    , "count" .= (1 :: Int)
                                    , "tags" .= ["external" :: Text]
                                    ]
                               ]
                        ] ::
                        Value
                    )
  where
    layerOf d p = viaNonEmpty head [m.layer | m <- d.modules, m.path == p]

-- | A module at a path, named and identified by it, importing modules by their paths.
at :: Text -> [Text] -> Module
at path = mkModuleAt path path

internal :: Text -> EdgeTarget
internal p = Resolved (moduleIdUnsafe p) (moduleNameUnsafe p :| [])
