module TypeScript.RenderableSpec (spec) where

import Test.Hspec
import TestUtils (renderGolden)
import TypeScript.AST

spec :: Spec
spec = do
    describe "Renderable TsNode" $ do
        it "renders Source as raw text" $
            renderGolden "ts-render-source" $
                Source "  \n  "

        it "renders Comment as raw text" $
            renderGolden "ts-render-comment" $
                Comment "// comment\n" " comment"

        it "renders Docs as raw text" $
            renderGolden "ts-render-docs" $
                Docs "/** doc */" " doc "

        it "renders Import as prefix <> target <> suffix" $
            renderGolden "ts-render-import" $
                Import "import x from '" "@/lib" "';"

    describe "Renderable [TsNode]" $ do
        it "renders empty list as empty text" $
            renderGolden @[TsNode] "ts-render-empty-list" []

        it "concatenates rendered nodes" $ do
            let nodes =
                    [ Source "a"
                    , Comment "// b" " b"
                    , Source "c"
                    ]
            renderGolden "ts-render-concat" nodes
