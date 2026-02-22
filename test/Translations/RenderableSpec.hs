module Translations.RenderableSpec (spec) where

import Test.Hspec
import TestUtils (renderGolden)
import Translations.Parser (TransTree (Branch, Leaf, Root))

spec :: Spec
spec = do
    describe "Renderable TransTree" $ do
        it "renders empty Root as {}" $
            renderGolden "trans-render-empty-root" $
                Root []

        it "renders single Leaf" $
            renderGolden "trans-render-single-leaf" $
                Root [Leaf "k" "v"]

        it "renders multiple Leaves" $
            renderGolden "trans-render-multiple-leaves" $
                Root [Leaf "a" "1", Leaf "b" "2"]

        it "renders nested Branch" $
            renderGolden "trans-render-nested-branch" $
                Root [Branch "nested" [Leaf "x" "y"]]

        it "renders mixed Root with Branch and Leaf" $
            renderGolden "trans-render-mixed-root" $
                Root [Leaf "top" "value", Branch "inner" [Leaf "k" "v"]]
