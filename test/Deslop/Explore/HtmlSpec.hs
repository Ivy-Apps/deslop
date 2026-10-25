module Deslop.Explore.HtmlSpec (spec) where

import Data.Text qualified as T
import Deslop.Explore.Html (explorerPage)
import Deslop.Explore.Html.Template (marker)
import Deslop.Explore.ModuleGraph (diagramOf)
import Fixtures.Deslop.Module (mkModuleAt)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Explore.Html" $ do
    describe "explorerPage" $ do
        it "carries the graph inline, with no marker left" $ do
            let page = explorerPage $ diagramOf Nothing Nothing mempty [mkModuleAt "src/a.ts" "src/a.ts" []]

            page `shouldSatisfy` T.isInfixOf "\"schema\":\"deslop-graph/1\""
            forM_ universe $ \slot -> page `shouldSatisfy` (not . T.isInfixOf (marker slot))

        it "carries every vendored library inline, as it is on disk" $ do
            let page = explorerPage $ diagramOf Nothing Nothing mempty []

            forM_ ["cytoscape.min.js", "elk.bundled.js", "cytoscape-elk.js"] $ \library -> do
                onDisk <- decodeUtf8 <$> readFileBS ("explorer/vendor/" <> library)
                page `shouldSatisfy` T.isInfixOf ("<script>" <> onDisk <> "</script>")

        it "fetches nothing, so it opens from disk" $ do
            let page = explorerPage $ diagramOf Nothing Nothing mempty []

            page `shouldSatisfy` (not . T.isInfixOf "<script src")
            page `shouldSatisfy` (not . T.isInfixOf "<link")

        it "keeps a module's name from closing the script that holds the graph" $ do
            let page = explorerPage $ diagramOf Nothing Nothing mempty [mkModuleAt "src/x.ts" "</script><b>" []]

            page `shouldSatisfy` T.isInfixOf "\\u003c/script>\\u003cb>"
            page `shouldSatisfy` (not . T.isInfixOf "</script><b>")

        it "carries each library's licence notice, id and source" $ do
            let page = explorerPage $ diagramOf Nothing Nothing mempty []

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
