module Deslop.Explore.ModuleGraph.JsonSpec (spec) where

import Data.Aeson (Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.Set qualified as Set
import Deslop.Explore.ModuleGraph (diagramOf)
import Deslop.Explore.ModuleGraph.Json (graphJson)
import Deslop.Module (
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    ModuleRole (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Deslop.Module qualified as M
import Fixtures.Deslop.Explore.ModuleGraph.Json (moduleTags, textField)
import Fixtures.Deslop.Module (mkEdge, mkModuleAt)
import Git.SourceLink (Branch (..), DiffAnchors (..), Forge (..), PullRequest (..), Revision (..), SourceBase (..), SourceLinks (..))
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Deslop.Explore.ModuleGraph.Json" $ do
    describe "graphJson" $ do
        it "tags a module by its role and whether it changed" $ do
            let d = diagramOf Nothing Nothing (Set.fromList [rp "t.ts", rp "b.ts"]) [testAt "t.ts", benchAt "b.ts", at "s.ts" []]

            moduleTags (graphJson d) `shouldBe` Just [("b.ts", ["bench", "changed"]), ("s.ts", []), ("t.ts", ["test", "changed"])]

        it "lists a module's tests by their paths" $ do
            let d = diagramOf Nothing Nothing mempty [at "src/a.ts" [], (at "src/a.test.ts" []) {M.role = Test (Just $ moduleIdUnsafe "src/a.ts")}]

            testedBy (graphJson d) `shouldBe` Just [("src/a.test.ts", []), ("src/a.ts", ["src/a.test.ts"])]

        it "writes nodes and edges in the deslop-graph/1 schema" $ do
            let m = (at "src/a.ts" []) {M.edges = [mkEdge "src/a.ts" 1 "react" (External $ moduleNameUnsafe "react") ImportEdge]}

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

            textField "sourceBase" (graphJson $ diagramOf (Just links) Nothing mempty [at "a.ts" []]) `shouldBe` Just (Just "https://gitlab.com/o/r/-/blob/main/")

        it "carries the pull request the diff is read in, and the project's folder in the repository" $ do
            let links =
                    SourceLinks
                        { source = SourceBase "https://github.com/o/r/blob/x/app/"
                        , revision = OnBranch (Branch "x")
                        , forge = GitHub DiffAnchors {repoPrefix = "app/"}
                        }
                graph = graphJson $ diagramOf (Just links) (Just $ PullRequest "https://github.com/o/r/pull/7") mempty [at "a.ts" []]

            (textField "pullRequest" graph, textField "repoPrefix" graph)
                `shouldBe` (Just (Just "https://github.com/o/r/pull/7"), Just (Just "app/"))

-- | A module at a path, named and identified by it, importing modules by their paths.
at :: Text -> [Text] -> Module
at path = mkModuleAt path path

testAt :: Text -> Module
testAt path = (at path []) {M.role = Test Nothing}

benchAt :: Text -> Module
benchAt path = (at path []) {M.role = Benchmark}

-- | Each module's id and the paths of its tests.
testedBy :: Value -> Maybe [(Text, [Text])]
testedBy = parseMaybe (withObject "graph" $ \o -> catMaybes <$> (traverse node =<< o .: "nodes"))
  where
    node = withObject "node" $ \n ->
        n .: "kind" >>= \case
            ("module" :: Text) -> Just <$> ((,) <$> n .: "id" <*> n .: "testedBy")
            _ -> pure Nothing
