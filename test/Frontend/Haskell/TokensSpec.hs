module Frontend.Haskell.TokensSpec (spec) where

import Frontend.Haskell.Tokens (HsToken (..), HsTokenKind (..), isKeyword, isTrivia)
import Test.Hspec

spec :: Spec
spec = describe "Frontend.Haskell.Tokens" $ do
    describe "trivia" $ do
        it "is whitespace, comments and pragmas, which layout looks straight through" $
            fmap isTrivia [HsToken " " WhitespaceK, HsToken "-- c" LineCommentK, HsToken "{- c -}" BlockCommentK, HsToken "{-# SOURCE #-}" PragmaK]
                `shouldBe` [True, True, True, True]

        it "is never a word, a symbol or a literal" $
            fmap isTrivia [HsToken "import" VarIdK, HsToken "-->" SymbolK, HsToken "\"--\"" StringK, HsToken "(" SpecialK]
                `shouldBe` [False, False, False, False]

    describe "keywords" $ do
        it "are variable identifiers spelled as the keyword" $
            isKeyword "import" (HsToken "import" VarIdK) `shouldBe` True

        it "are not a string that spells one" $
            isKeyword "import" (HsToken "\"import\"" StringK) `shouldBe` False

        it "are not a longer identifier that starts with one" $
            isKeyword "import" (HsToken "imports" VarIdK) `shouldBe` False
