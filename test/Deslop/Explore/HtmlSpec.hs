module Deslop.Explore.HtmlSpec (spec) where

import Data.Text qualified as T
import Deslop.Explore.Html (explorerPage)
import Deslop.Explore.Html.Template (marker)
import Deslop.Explore.ModuleGraph (diagramOf)
import Deslop.Explore.ModuleGraph.Json (graphJson)
import Fixtures.Deslop.Explore.Html (graphIn, titleIn)
import Fixtures.Deslop.Module (mkModuleAt)
import Git.SourceLink (ProjectName (..))
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Explore.Html" $ do
    describe "explorerPage" $ do
        it "carries the graph inline, with no marker left" $ do
            let diagram = diagramOf Nothing Nothing Nothing [mkModuleAt "src/a.ts" "src/a.ts" []]
                page = pageOf diagram

            graphIn page `shouldBe` Just (graphJson diagram)
            forM_ universe $ \slot -> page `shouldSatisfy` (not . T.isInfixOf (marker slot))

        it "carries every script inline, as it is on disk, in load order" $ do
            let page = pageOf $ diagramOf Nothing Nothing Nothing []
                inOrder =
                    ["vendor/cytoscape.min.js", "vendor/elk.bundled.js", "vendor/cytoscape-elk.js"]
                        <> fmap ("src/" <>) ["graph.js", "colour.js", "view.js", "sha256.js", "search.js", "navigation.js", "explorer.js"]

            offsets <- forM inOrder $ \file -> do
                onDisk <- decodeUtf8 <$> readFileBS ("explorer/" <> file)
                let (preceding, at) = T.breakOn ("<script>" <> onDisk <> "</script>") page
                at `shouldSatisfy` (not . T.null)
                pure $ T.length preceding
            offsets `shouldSatisfy` (\os -> and $ zipWith (<) os (drop 1 os))

        it "fetches nothing, so it opens from disk" $ do
            let page = pageOf $ diagramOf Nothing Nothing Nothing []

            page `shouldSatisfy` (not . T.isInfixOf "<script src")
            page `shouldSatisfy` (not . T.isInfixOf "<link")

        it "keeps a module's name from closing the script that holds the graph" $ do
            let diagram = diagramOf Nothing Nothing Nothing [mkModuleAt "src/x.ts" "</script><b>" []]
                page = pageOf diagram

            graphIn page `shouldBe` Just (graphJson diagram)
            page `shouldSatisfy` (not . T.isInfixOf "</script><b>")

        it "carries each library's licence notice, id and source" $ do
            let page = pageOf $ diagramOf Nothing Nothing Nothing []

            forM_
                [ -- cytoscape.min.js carries its own MIT notice.
                  "Copyright (c) 2016-2026, The Cytoscape Consortium."
                , "elkjs 0.9.3, SPDX-License-Identifier: EPL-2.0"
                , "https://github.com/kieler/elkjs/tree/0.9.3"
                , "https://www.eclipse.org/legal/epl-2.0/"
                , "cytoscape-elk 2.3.0, SPDX-License-Identifier: MIT"
                , "https://github.com/cytoscape/cytoscape.js-elk"
                , "Copyright (c) 2016-2018, 2020, The Cytoscape Consortium."
                ]
                $ \notice -> page `shouldSatisfy` T.isInfixOf notice

        it "titles the page after the project" $
            titleOf "deslop" `shouldBe` Just "deslop graph"

        it "escapes a name that would otherwise close the title or start an entity" $
            titleOf "</title><b>&amp;" `shouldBe` Just "&lt;/title&gt;&lt;b&gt;&amp;amp; graph"
  where
    pageOf = explorerPage (ProjectName "deslop")
    titleOf name = titleIn . explorerPage (ProjectName name) $ diagramOf Nothing Nothing Nothing []
