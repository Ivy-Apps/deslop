module Deslop.Explore.ModuleGraphSpec (spec) where

import Data.Aeson (Key, Value, decode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
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
import Git.SourceLink (Branch (..), DiffAnchors (..), Forge (..), PullRequest (..), Revision (..), SourceBase (..), SourceLinks (..))
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

            [m.path | m <- d.modules, m.hub] `shouldBe` ["src/Path.hs"]

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
                `shouldBe` [("perf/a.ts", Benchmark), ("src/a.check.ts", Test), ("src/a.ts", Source)]

        it "marks the modules whose files changed" $ do
            let d = diagramOf Nothing Nothing (Set.fromList [rp "src/b.ts", rp "README.md"]) [at "src/a.ts" [], at "src/b.ts" []]

            [(m.path, m.changed) | m <- d.modules] `shouldBe` [("src/a.ts", False), ("src/b.ts", True)]

        it "links a module to the specs named after it" $ do
            let d =
                    diagramOf
                        Nothing
                        Nothing
                        mempty
                        [ at "src/Deslop/Diagram.hs" []
                        , at "src/Other/Diagram.hs" []
                        , at "src/Deslop.hs" []
                        , testAt "test/Deslop/DiagramSpec.hs" []
                        , testAt "test/Deslop/DiagramPropSpec.hs" []
                        , testAt "test/DeslopSpec.hs" []
                        , testAt "test/Doubles/CLI.hs" []
                        ]

            [(m.path, m.testedBy) | m <- d.modules, m.role == Source]
                `shouldBe` [ ("src/Deslop/Diagram.hs", ["test/Deslop/DiagramPropSpec.hs", "test/Deslop/DiagramSpec.hs"])
                           , ("src/Deslop.hs", ["test/DeslopSpec.hs"])
                           , ("src/Other/Diagram.hs", [])
                           ]

        it "names every folder above a module, each under its parent" $ do
            let d = diagramOf Nothing Nothing mempty [at "src/ui/Button.tsx" [], at "main.ts" []]

            d.folders
                `shouldBe` [ FolderNode {path = FolderPath ("src" :| []), parent = Nothing}
                           , FolderNode {path = FolderPath ("src" :| ["ui"]), parent = Just (FolderPath ("src" :| []))}
                           ]

    describe "JSON" $ do
        it "tags a module by its role and whether it changed" $ do
            let d = diagramOf Nothing Nothing (Set.fromList [rp "t.ts", rp "b.ts"]) [testAt "t.ts" [], benchAt "b.ts" [], at "s.ts" []]

            moduleTags d `shouldBe` Just [("b.ts", ["bench", "changed"]), ("s.ts", []), ("t.ts", ["test", "changed"])]

        it "writes nodes and edges in the deslop-graph/1 schema" $ do
            let m = (at "src/a.ts" []) {M.edges = [mkEdge "src/a.ts" 1 "react" (External $ moduleNameUnsafe "react") ImportEdge]}

            decode (encode $ diagramOf Nothing Nothing mempty [m])
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
                        , "sourceBase" .= (Nothing :: Maybe Text)
                        , "pullRequest" .= (Nothing :: Maybe Text)
                        , "repoPrefix" .= (Nothing :: Maybe Text)
                        ] ::
                        Value
                    )

        it "carries where the project's files can be read on the web" $ do
            let links = SourceLinks {source = SourceBase "https://gitlab.com/o/r/-/blob/main/", revision = OnBranch (Branch "main"), forge = GitLab}

            fieldIn "sourceBase" (diagramOf (Just links) Nothing mempty [at "a.ts" []]) `shouldBe` Just (Just "https://gitlab.com/o/r/-/blob/main/")

        it "carries the pull request the diff is read in, and the project's folder in the repository" $ do
            let links =
                    SourceLinks
                        { source = SourceBase "https://github.com/o/r/blob/x/app/"
                        , revision = OnBranch (Branch "x")
                        , forge = GitHub DiffAnchors {repoPrefix = "app/"}
                        }
                diagram = diagramOf (Just links) (Just $ PullRequest "https://github.com/o/r/pull/7") mempty [at "a.ts" []]

            (fieldIn "pullRequest" diagram, fieldIn "repoPrefix" diagram)
                `shouldBe` (Just (Just "https://github.com/o/r/pull/7"), Just (Just "app/"))
  where
    layerOf d p = viaNonEmpty head [m.layer | m <- d.modules, m.path == p]

-- | A module at a path, named and identified by it, importing modules by their paths.
at :: Text -> [Text] -> Module
at path = mkModuleAt path path

-- | A top-level text field of the graph, as the page reads it.
fieldIn :: Key -> Diagram -> Maybe (Maybe Text)
fieldIn key = parseMaybe (withObject "graph" (.: key)) <=< decode . encode

-- | Each module's id and tags, as the page reads them.
moduleTags :: Diagram -> Maybe [(Text, [Text])]
moduleTags = parseMaybe (withObject "graph" $ \o -> catMaybes <$> (traverse node =<< o .: "nodes")) <=< decode . encode
  where
    node = withObject "node" $ \n ->
        n .: "kind" >>= \case
            ("module" :: Text) -> Just <$> ((,) <$> n .: "id" <*> n .: "tags")
            _ -> pure Nothing

testAt :: Text -> [Text] -> Module
testAt path deps = (at path deps) {M.role = Test}

benchAt :: Text -> [Text] -> Module
benchAt path deps = (at path deps) {M.role = Benchmark}

internal :: Text -> EdgeTarget
internal p = Resolved (moduleIdUnsafe p) (moduleNameUnsafe p :| [])
