module TypeScript.LexerSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (errorBundlePretty, parse)

import Control.Monad
import TypeScript.Lexer (lexer)
import TypeScript.Tokens

spec :: Spec
spec = do
    describe "TypeScript Lexer" $ do
        it "reconstructs the original input exactly (Round Trip)" $ do
            property prop_roundTrip

    describe "Import Parser" $ do
        let runTest input = parse lexer "test.ts" input
        let cases =
                [
                    ( "Basic single quotes"
                    , "import { foo } from 'bar'; const x = 1;"
                    , "import { foo } from 'bar';"
                    )
                ,
                    ( "Basic double quotes"
                    , "import * as React from \"react\";\nconsole.log();"
                    , "import * as React from \"react\";"
                    )
                ,
                    ( "Multiline"
                    , T.unlines
                        [ "import {"
                        , "  urls,"
                        , "  labels,"
                        , "} from '../../lib/constants';"
                        , "\n\n"
                        ]
                    , T.unlines
                        [ "import {"
                        , "  urls,"
                        , "  labels,"
                        , "} from '../../lib/constants';"
                        ]
                    )
                ,
                    ( "Multiline with trailing comma"
                    , T.unlines
                        [ "import {"
                        , "  foo,"
                        , "  bar,"
                        , "} from 'baz'; export const x = 1;"
                        ]
                    , T.unlines
                        [ "import {"
                        , "  foo,"
                        , "  bar,"
                        , "} from 'baz';"
                        ]
                    )
                ,
                    ( "Strings containing braces"
                    , "import { \"}\" as brace } from 'lib'; const y = 2;"
                    , "import { \"}\" as brace } from 'lib';"
                    )
                ,
                    ( "Strings containing semicolons"
                    , "import { \";\" as semi } from 'lib'; "
                    , "import { \";\" as semi } from 'lib';"
                    )
                ,
                    ( "Block comments inside"
                    , "import { /* } */ a } from 'b'; "
                    , "import { /* } */ a } from 'b';"
                    )
                ,
                    ( "Line comments inside (multiline)"
                    , "import {\n  a, // comment with }\n} from 'b'; "
                    , "import {\n  a, // comment with }\n} from 'b';"
                    )
                ,
                    ( "Terminated by newline (no semicolon)"
                    , "import { x } from 'y'\nconst z = 1;"
                    , "import { x } from 'y'\n"
                    )
                ,
                    ( "Await import terminated by ')'"
                    , T.unlines
                        [ "return {"
                        , "  locale,"
                        , "  strings: await import (`../../strings/${local}.json`)).default,"
                        , "};"
                        ]
                    , "import (`../../strings/${local}.json`)"
                    )
                ,
                    ( "Await import terminated by ';'"
                    , "await import ('./lib/login');\nlet x = 42"
                    , "import ('./lib/login');"
                    )
                ]

        forM_ cases $ \(desc, input, expectedRaw) ->
            it ("parses: " <> desc) $ do
                -- When
                let res = head . filter (\n -> n.kind == ImportK) <$> runTest input

                -- Then
                case res of
                    Left err -> expectationFailure (errorBundlePretty err)
                    Right token -> do
                        token.kind `shouldBe` ImportK
                        T.strip (token.raw) `shouldBe` T.strip expectedRaw

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
        , (2, genDocs)
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

genDocs :: Gen Text
genDocs = do
    content <- T.pack <$> listOf contentChar
    pure $ "/** " <> content <> " */"

genWhitespace :: Gen Text
genWhitespace = T.pack <$> listOf1 contentChar

genRaw :: Gen Text
genRaw = T.pack <$> listOf1 contentChar

contentChar :: Gen Char
contentChar = elements (['a' .. 'z'] <> ['0' .. '9'] <> ['=', '+', '-', '(', ')'])