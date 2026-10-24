module Frontend.Haskell.LexerSpec (spec) where

import Data.Text qualified as T
import Frontend.Haskell.Lexer (lexHaskell)
import Frontend.Haskell.Tokens (HsToken (..), HsTokenKind (..))
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

-- | Each token's kind and text, without the whitespace between them.
lexed :: Text -> [(HsTokenKind, Text)]
lexed = fmap (\t -> (t.kind, t.raw)) . filter ((/= WhitespaceK) . (.kind)) . lexHaskell

spec :: Spec
spec = describe "Frontend.Haskell.Lexer" $ do
    describe "losslessness" $ do
        prop "gives back any text when its tokens are concatenated" $ do
            source <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
            foldMap (.raw) (lexHaskell source) === source

        -- Haskell's own characters, dense enough to open every kind of token
        -- and leave most of them unterminated.
        prop "gives back Haskell-shaped text when its tokens are concatenated" $ do
            source <- forAll $ Gen.text (Range.linear 0 200) (Gen.element ("{}-#\"'\\\n\t aZ.:|>" :: String))
            foldMap (.raw) (lexHaskell source) === source

        -- A token that is not otherwise recognised is one character, so no
        -- malformed construct can swallow the rest of the file into one token.
        prop "reads what it does not recognise one character at a time" $ do
            source <- forAll $ Gen.text (Range.linear 0 200) (Gen.element ("{}-#\"'\\\n\t aZ.:|>" :: String))
            [t.raw | t <- lexHaskell source, t.kind == OtherK, T.length t.raw /= 1] === []

        it "lexes nothing out of nothing" $
            lexHaskell "" `shouldBe` []

    describe "the dash rule" $ do
        it "reads two dashes as a comment to the end of the line" $
            lexed "-- import A\nx" `shouldBe` [(LineCommentK, "-- import A"), (VarIdK, "x")]

        it "reads any longer run of dashes as a comment" $
            lexed "---- import A" `shouldBe` [(LineCommentK, "---- import A")]

        it "reads a Haddock comment as a comment" $
            lexed "-- | Docs" `shouldBe` [(LineCommentK, "-- | Docs")]

        it "reads dashes followed by another symbol as an operator" $
            lexed "a --| b" `shouldBe` [(VarIdK, "a"), (SymbolK, "--|"), (VarIdK, "b")]

        it "reads an arrow made of dashes as an operator" $
            lexed "a --> b" `shouldBe` [(VarIdK, "a"), (SymbolK, "-->"), (VarIdK, "b")]

        it "reads a symbol before the dashes as part of the operator" $
            lexed "a |-- b" `shouldBe` [(VarIdK, "a"), (SymbolK, "|--"), (VarIdK, "b")]

        it "reads a single dash as an operator" $
            lexed "a - b" `shouldBe` [(VarIdK, "a"), (SymbolK, "-"), (VarIdK, "b")]

    describe "block comments" $ do
        it "nest, so an inner close does not end the outer comment" $
            lexed "{- a {- b -} import A -} x" `shouldBe` [(BlockCommentK, "{- a {- b -} import A -}"), (VarIdK, "x")]

        it "run to the end of the input when never closed" $
            lexed "{- import A" `shouldBe` [(BlockCommentK, "{- import A")]

        it "are not opened by a brace alone" $
            lexed "{ x }" `shouldBe` [(SpecialK, "{"), (VarIdK, "x"), (SpecialK, "}")]

    describe "pragmas" $ do
        it "are one token from {-# to #-}" $
            lexed "{-# LANGUAGE CPP #-}\nmodule" `shouldBe` [(PragmaK, "{-# LANGUAGE CPP #-}"), (VarIdK, "module")]

        it "span lines" $
            lexed "{-# LANGUAGE\n  CPP #-}" `shouldBe` [(PragmaK, "{-# LANGUAGE\n  CPP #-}")]

        it "run to the end of the input when never closed" $
            lexed "{-# SOURCE" `shouldBe` [(PragmaK, "{-# SOURCE")]

    describe "literals" $ do
        it "keep a comment opener inside a string" $
            lexed "\"-- {- x\" y" `shouldBe` [(StringK, "\"-- {- x\""), (VarIdK, "y")]

        it "keep an escaped quote inside a string" $
            lexed "\"a\\\"b\" c" `shouldBe` [(StringK, "\"a\\\"b\""), (VarIdK, "c")]

        it "keep a string gap inside a string" $
            lexed "\"a\\\n   \\b\" c" `shouldBe` [(StringK, "\"a\\\n   \\b\""), (VarIdK, "c")]

        -- Regression: the escape consumed the backslash and then failed on the
        -- newline, and the whole file fell back to a single token.
        it "end a string before a backslash that ends the line, and go on" $
            lexed "s = \"a\\\nimport C" `shouldBe` [(VarIdK, "s"), (SymbolK, "="), (StringK, "\"a"), (SymbolK, "\\"), (VarIdK, "import"), (ConIdK, "C")]

        it "end an unterminated string at the end of its line" $
            lexed "\"abc\nx" `shouldBe` [(StringK, "\"abc"), (VarIdK, "x")]

        it "read a character literal" $
            lexed "'\"' x" `shouldBe` [(CharK, "'\"'"), (VarIdK, "x")]

        it "read an escaped character literal" $
            lexed "'\\'' '\\n' '\\SOH'" `shouldBe` [(CharK, "'\\''"), (CharK, "'\\n'"), (CharK, "'\\SOH'")]

        it "leave a promoted constructor's quote to itself" $
            lexed "'Just" `shouldBe` [(OtherK, "'"), (ConIdK, "Just")]

    describe "identifiers" $ do
        it "read a dotted module name as one token" $
            lexed "Data.Map.Strict" `shouldBe` [(ConIdK, "Data.Map.Strict")]

        it "stop a module name before a qualified variable" $
            lexed "T.pack" `shouldBe` [(ConIdK, "T"), (SymbolK, "."), (VarIdK, "pack")]

        it "keep primes and underscores in a name" $
            lexed "x' _y M'" `shouldBe` [(VarIdK, "x'"), (VarIdK, "_y"), (ConIdK, "M'")]

        it "do not split a keyword out of a longer name" $
            lexed "imports" `shouldBe` [(VarIdK, "imports")]

    describe "the start of a file" $ do
        it "reads a byte-order mark as whitespace" $
            fmap (.kind) (lexHaskell "\xFEFFmodule") `shouldBe` [WhitespaceK, VarIdK]

        it "reads a #! line as a comment" $
            lexed "#!/usr/bin/env runghc\nimport A" `shouldBe` [(LineCommentK, "#!/usr/bin/env runghc"), (VarIdK, "import"), (ConIdK, "A")]

        it "reads #! anywhere else as an operator" $
            lexed "x #! y" `shouldBe` [(VarIdK, "x"), (SymbolK, "#!"), (VarIdK, "y")]
