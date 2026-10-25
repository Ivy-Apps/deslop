module Deslop.Explore.ModuleGraph.JsonSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Set qualified as Set
import Deslop.Explore.ModuleGraph (diagramOf)
import Deslop.Explore.ModuleGraph.Json (graphJson)
import Deslop.Module (
    EdgeKind (..),
    EdgeTarget (..),
    moduleNameUnsafe,
 )
import Deslop.Module qualified as M
import Fixtures.Deslop.Explore.ModuleGraph.Json (moduleField, moduleTags, textField)
import Fixtures.Deslop.Module (mkBenchAt, mkEdge, mkPathModule, mkTestAt, mkTestOf)
import Git.SourceLink (Branch (..), DiffAnchors (..), Forge (..), PullRequest (..), Revision (..), SourceBase (..), SourceLinks (..))
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Deslop.Explore.ModuleGraph.Json" $ do
    describe "graphJson" $ do
        it "tags a module by its role and whether it changed" $ do
            let d = diagramOf Nothing Nothing (Set.fromList [rp "t.ts", rp "b.ts"]) [mkTestAt "t.ts" [], mkBenchAt "b.ts" [], mkPathModule "s.ts" []]

            moduleTags (graphJson d) `shouldBe` Just [("b.ts", ["bench", "changed"]), ("s.ts", []), ("t.ts", ["test", "changed"])]

        it "lists a module's tests by their paths" $ do
            let d = diagramOf Nothing Nothing mempty [mkPathModule "src/a.ts" [], mkTestOf "src/a.test.ts" "src/a.ts"]

            moduleField @[Text] "testedBy" (graphJson d) `shouldBe` Just [("src/a.test.ts", []), ("src/a.ts", ["src/a.test.ts"])]

        it "writes nodes and edges in the deslop-graph/1 schema" $ do
            let m = (mkPathModule "src/a.ts" []) {M.edges = [mkEdge "src/a.ts" 1 "react" (External $ moduleNameUnsafe "react") ImportEdge]}

            graphJson (diagramOf Nothing Nothing mempty [m])
                `shouldBe` object
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
                    , "hub" .= object ["share" .= (0.4 :: Double), "minimum" .= (3 :: Int)]
                    ]

        it "carries where the project's files can be read on the web" $ do
            let links = SourceLinks {source = SourceBase "https://gitlab.com/o/r/-/blob/main/", revision = OnBranch (Branch "main"), forge = GitLab}

            textField "sourceBase" (graphJson $ diagramOf (Just links) Nothing mempty [mkPathModule "a.ts" []]) `shouldBe` Just (Just "https://gitlab.com/o/r/-/blob/main/")

        it "carries the pull request the diff is read in, and the project's folder in the repository" $ do
            let links =
                    SourceLinks
                        { source = SourceBase "https://github.com/o/r/blob/x/app/"
                        , revision = OnBranch (Branch "x")
                        , forge = GitHub DiffAnchors {repoPrefix = "app/"}
                        }
                graph = graphJson $ diagramOf (Just links) (Just $ PullRequest "https://github.com/o/r/pull/7") mempty [mkPathModule "a.ts" []]

            (textField "pullRequest" graph, textField "repoPrefix" graph)
                `shouldBe` (Just (Just "https://github.com/o/r/pull/7"), Just (Just "app/"))
