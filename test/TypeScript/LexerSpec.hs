module TypeScript.LexerSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (errorBundlePretty, parse)

import TypeScript.Lexer (lexer)
import TypeScript.Tokens

spec :: Spec
spec = describe "TypeScript Lexer" $ do
    it "reconstructs the original input exactly (Round Trip)" $ do
        property prop_roundTrip

-- | The core property: Reassembled tokens must match the original input exactly.
prop_roundTrip :: TsInput -> Property
prop_roundTrip (TsInput input) =
    let result = parse lexer "test.ts" input
     in case result of
            Left err ->
                counterexample ("Parse failed:\n" <> errorBundlePretty err) False
            Right tokens ->
                let reconstructed = T.concat ((.raw) <$> tokens)
                 in counterexample
                        ("Expected:\n" <> show input <> "\nBut got:\n" <> show reconstructed)
                        (reconstructed == input)

newtype TsInput = TsInput Text deriving (Show, Eq)

instance Arbitrary TsInput where
    arbitrary = TsInput . T.concat <$> listOf genChunk

genChunk :: Gen Text
genChunk =
    frequency
        [ (5, genImport)
        , (2, genLineComment)
        , (2, genBlockComment)
        , (5, genWhitespace)
        , (6, genRaw)
        ]

genImport :: Gen Text
genImport = do
    ws1 <- genWhitespace
    ws2 <- genWhitespace
    body <- genImportBody
    pure $ "import" <> ws1 <> body <> ws2 <> ";"

genImportBody :: Gen Text
genImportBody = do
    content <-
        listOf $
            frequency
                [ (5, elements ["foo", "bar", "Baz", ",", "\n"])
                , (1, pure "{")
                , (1, pure "}")
                , (1, genStringLiteral)
                ]
    pure $ "{" <> T.concat content <> "} from 'lib'"

genStringLiteral :: Gen Text
genStringLiteral = do
    quote <- elements ["\"", "'", "`"]
    content <-
        listOf $
            frequency
                [ (10, elements ["a", "b", "c", " ", "{", "}", ";"])
                , (1, pure ("\\" <> quote)) -- Escaped quote
                ]
    pure $ quote <> T.concat content <> quote

genLineComment :: Gen Text
genLineComment = do
    content <- T.pack <$> listOf contentChar
    pure $ "// " <> content <> "\n"

genBlockComment :: Gen Text
genBlockComment = do
    content <- T.pack <$> listOf contentChar
    pure $ "/* " <> content <> " */"

genWhitespace :: Gen Text
genWhitespace = T.pack <$> listOf1 contentChar

genRaw :: Gen Text
genRaw = T.pack <$> listOf1 contentChar

contentChar :: Gen Char
contentChar = elements (['a' .. 'z'] <> ['0' .. '9'] <> ['=', '+', '-', '(', ')'])