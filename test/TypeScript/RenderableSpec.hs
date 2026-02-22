module TypeScript.RenderableSpec (spec) where

import Test.Hspec
import Types (Renderable (render))
import TypeScript.AST
import TypeScript.Parser

spec :: Spec
spec = do
  describe "Renderable TsNode" $ do
    it "renders Source as raw text" $
      render (Source "  \n  ") `shouldBe` "  \n  "

    it "renders Comment as raw text" $
      render (Comment "// comment\n" " comment") `shouldBe` "// comment\n"

    it "renders Docs as raw text" $
      render (Docs "/** doc */" " doc ") `shouldBe` "/** doc */"

    it "renders Import as prefix <> target <> suffix" $
      render (Import "import x from '" "@/lib" "';")
        `shouldBe` "import x from '" <> "@/lib" <> "';"

  describe "Renderable [TsNode]" $ do
    it "renders empty list as empty text" $
      render @[TsNode] [] `shouldBe` ""

    it "concatenates rendered nodes" $ do
      let nodes =
            [ Source "a"
            , Comment "// b" " b"
            , Source "c"
            ]
      render nodes `shouldBe` "a// bc"

    it "round-trips with parse for import-only snippet" $ do
      let snippet = "import * from '@/lib/utils'"
      let file = TsFile "x.ts" snippet
      case parseTs file of
        Left _ -> pure ()
        Right prog -> render prog.ast `shouldBe` snippet
