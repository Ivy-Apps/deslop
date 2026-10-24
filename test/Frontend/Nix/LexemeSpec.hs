module Frontend.Nix.LexemeSpec (spec) where

import Frontend.Nix.Lexeme (Lexeme (..), Part (..), Quoting (..), decodeChunk, isTrivia)
import Renderable qualified
import Test.Hspec

spec :: Spec
spec = describe "Frontend.Nix.Lexeme" $ do
    describe "rendering" $ do
        it "gives a string back with its delimiters and interpolations" $
            Renderable.render [Str DoubleQuoted [Chunk "a", Interpolation [Identifier "b"], Chunk "c"], Str Indented [Chunk "d"]]
                `shouldBe` "\"a${b}c\"''d''"

        it "gives a path and a search path back as written" $
            Renderable.render [PathLit [Chunk "./p/", Interpolation [Identifier "n"], Chunk ".nix"], Trivia " ", SearchPath "nixpkgs"]
                `shouldBe` "./p/${n}.nix <nixpkgs>"

    describe "the text a chunk stands for" $ do
        it "undoes a double-quoted string's escapes" $
            decodeChunk DoubleQuoted "a\\\"b\\nc\\$d" `shouldBe` "a\"b\nc$d"

        it "undoes an indented string's escapes" $
            decodeChunk Indented "a'''b''$c''\\nd" `shouldBe` "a''b$c\nd"

        it "leaves text without escapes alone" $
            decodeChunk DoubleQuoted "/modules/x.nix" `shouldBe` "/modules/x.nix"

    it "counts whitespace and comments as trivia, and nothing else" $
        fmap isTrivia [Trivia "# x", Identifier "x", Punctuation "."] `shouldBe` [True, False, False]
