module Frontend.TypeScript.CSTSpec (spec) where

import Frontend.TypeScript.CST
import Test.Hspec
import TestUtils (renderGolden)

spec :: Spec
spec = describe "Frontend.TypeScript.CST" $ do
    describe "Renderable TsNode" $ do
        it "renders Source as raw text" $
            renderGolden "ts-render-source" $
                Source "fun main() {\n  console.log('Hello, world!')\n}\n"

        it "renders Import as prefix <> target <> suffix" $
            renderGolden "ts-render-import" $
                Import "import x from '" "@/lib" "';"

    describe "Renderable [TsNode]" $ do
        it "renders empty list as empty text" $
            renderGolden @[TsNode] "ts-render-empty-list" []

        it "concatenates rendered nodes" $ do
            let nodes =
                    [ Source "a"
                    , Source "// b"
                    , Source "c"
                    ]
            renderGolden "ts-render-concat" nodes
