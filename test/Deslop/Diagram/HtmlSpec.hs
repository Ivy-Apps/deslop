module Deslop.Diagram.HtmlSpec (spec) where

import Data.Text qualified as T
import Deslop.Diagram (diagramOf)
import Deslop.Diagram.Html (explorerPage, graphMarker, librariesMarker)
import Fixtures.Deslop.Module (mkModuleAt)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Diagram.Html" $ do
    describe "explorerPage" $ do
        it "carries the graph and every script inline, with no marker left" $ do
            let page = explorerPage $ diagramOf [mkModuleAt "src/a.ts" "src/a.ts" []]

            page `shouldSatisfy` T.isInfixOf "\"schema\":\"deslop-graph/1\""
            page `shouldSatisfy` T.isInfixOf "cytoscape"
            page `shouldSatisfy` (not . T.isInfixOf graphMarker)
            page `shouldSatisfy` (not . T.isInfixOf librariesMarker)

        it "fetches nothing, so it opens from disk" $ do
            let page = explorerPage $ diagramOf []

            page `shouldSatisfy` (not . T.isInfixOf "<script src")
            page `shouldSatisfy` (not . T.isInfixOf "<link")

        it "keeps a module's name from closing the script that holds the graph" $ do
            let page = explorerPage $ diagramOf [mkModuleAt "src/x.ts" "</script><b>" []]

            page `shouldSatisfy` T.isInfixOf "\\u003c/script>\\u003cb>"
            page `shouldSatisfy` (not . T.isInfixOf "</script><b>")
