module Deslop.Explore.Html.TemplateSpec (spec) where

import Data.Text qualified as T
import Deslop.Explore.Html.Template (Slot (..), fillTemplate, marker, parseTemplate)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Explore.Html.Template" $ do
    it "fills each slot where the template marks it" $ do
        let page = "<head>" <> marker Libraries <> "</head><body>" <> marker Graph <> "</body>"

        fmap (fillTemplate filled) (parseTemplate page)
            `shouldBe` Right "<head>[Libraries]</head><body>[Graph]</body>"

    it "fills a slot the template marks before another" $ do
        let page = marker Graph <> "|" <> marker Libraries

        fmap (fillTemplate filled) (parseTemplate page) `shouldBe` Right "[Graph]|[Libraries]"

    it "leaves content that spells a marker as it is" $ do
        let page = marker Libraries <> marker Graph

        fmap (fillTemplate marker) (parseTemplate page) `shouldBe` Right page

    it "refuses a template missing a marker, naming it" $
        reasonFor (parseTemplate $ "<body>" <> marker Libraries <> "</body>")
            `shouldSatisfy` T.isInfixOf (marker Graph)

    it "refuses a template marking a slot twice, naming it" $
        reasonFor (parseTemplate $ marker Libraries <> marker Graph <> marker Graph)
            `shouldSatisfy` T.isInfixOf ("more than one " <> marker Graph)
  where
    filled slot = "[" <> show slot <> "]"
    reasonFor = either toText (const "")
