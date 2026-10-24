module Deslop.Diagram.D2Spec (spec) where

import Deslop.Diagram.D2 (Document (..), Statement (..), Value (..), keyword, name, quote, render)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Diagram.D2" $ do
    describe "quote" $ do
        it "keeps a Next.js dynamic segment from reading as D2 syntax" $
            quote "[id]" `shouldBe` "\"[id]\""

        it "keeps an npm scope intact" $
            quote "@scope" `shouldBe` "\"@scope\""

        it "keeps a reserved word from reading as D2's own" $
            quote "label" `shouldBe` "\"label\""

        it "escapes the double quote and the backslash" $
            quote "a\"b\\c" `shouldBe` "\"a\\\"b\\\\c\""

    describe "render" $ do
        it "nests a file in its folders without the dot in its name nesting it further" $ do
            let doc = Document [Block (name $ "features" :| ["users", "page.tsx"]) []]

            render doc `shouldBe` "\"features\".\"users\".\"page.tsx\"\n"

        it "keeps a folder named like one of D2's boards from becoming one" $ do
            let doc = Document [Block (name $ "domain" :| ["Scenarios", "x.ts"]) []]

            render doc `shouldBe` "\"domain\".\"Scenarios\x200B\".\"x.ts\"\n"

        it "writes D2's own vocabulary bare and the project's names quoted" $ do
            let doc =
                    Document
                        [ Block
                            (name $ "a" :| [])
                            [ Field (keyword $ "style" :| ["stroke-width"]) (Bare "2")
                            , Field (keyword $ "label" :| []) (Str "a/b")
                            ]
                        , Connection (name $ "a" :| []) (name $ "b" :| []) []
                        ]

            render doc
                `shouldBe` "\"a\": {\n  style.stroke-width: 2\n  label: \"a/b\"\n}\n\"a\" -> \"b\"\n"
