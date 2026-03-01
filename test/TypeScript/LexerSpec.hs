module TypeScript.LexerSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, PropertyT, failure, footnote, forAll, (===))
import qualified Hedgehog.Gen as HGen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (errorBundlePretty, parse)

import Control.Monad
import TypeScript.Lexer (lexer)
import TypeScript.Tokens
import Utils (headOrThrow)

spec :: Spec
spec = do
    describe "TypeScript Lexer" $ do
        it "reconstructs the original input exactly (Round Trip)" $
            hedgehog prop_roundTrip

    describe "Import Parser" $ do
        let runTest = parse lexer "test.ts"
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
                    , "const module = await import ('./heavy-module');\nlet x = 42"
                    , "import ('./heavy-module');"
                    )
                ,
                    ( "Combined Default + Named/Namespace"
                    , "import React, { useState } from \"react\";"
                    , "import React, { useState } from \"react\";"
                    )
                ,
                    ( "Side-Effect Import"
                    , "import './styles.css';"
                    , "import './styles.css';"
                    )
                ,
                    ( "Type-Only Imports (Top level)"
                    , "import type { User, Role } from './models'; const x = 1;"
                    , "import type { User, Role } from './models';"
                    )
                ,
                    ( "Inline Type Imports (TS 4.5+)"
                    , "import { createStore, type Store } from 'redux'; "
                    , "import { createStore, type Store } from 'redux';"
                    )
                ,
                    ( "Import Attributes/Assertions (JSON)"
                    , "import data from './data.json' with { type: \"json\" }; "
                    , "import data from './data.json' with { type: \"json\" };"
                    )
                ,
                    ( "Named Import with Aliasing"
                    , "import { originalName as aliasName } from 'lib'; "
                    , "import { originalName as aliasName } from 'lib';"
                    )
                ,
                    ( "Namespace Import (Explicit)"
                    , "import * as Utils from './utils'; "
                    , "import * as Utils from './utils';"
                    )
                ,
                    ( "Keywords as Identifiers (Aliased)"
                    , "import { class as classSelector, delete as remove } from 'dom'; "
                    , "import { class as classSelector, delete as remove } from 'dom';"
                    )
                ,
                    ( "Empty Named Import (Side-effect intent)"
                    , "import {} from './init-module'; "
                    , "import {} from './init-module';"
                    )
                ,
                    ( "String Literal Export Names (Arbitrary Module Namespace)"
                    , "import { \"stupid-name\" as normal } from 'weird-lib'; "
                    , "import { \"stupid-name\" as normal } from 'weird-lib';"
                    )
                ]

        forM_ cases $ \(desc, input, expectedRaw) ->
            it ("parses: " <> desc) $ do
                -- When
                let res = headOrThrow . filter (\n -> n.kind == ImportK) <$> runTest input

                -- Then
                case res of
                    Left err -> expectationFailure (errorBundlePretty err)
                    Right token -> do
                        token.kind `shouldBe` ImportK
                        T.strip token.raw `shouldBe` T.strip expectedRaw

-- | The core property: Reassembled tokens must match the original input exactly.
prop_roundTrip :: PropertyT IO ()
prop_roundTrip = do
    input <- forAll genTsInput
    let result = parse lexer "test.ts" input
    case result of
        Left err -> do
            footnote (errorBundlePretty err)
            failure
        Right tokens -> do
            let reconstructed = T.concat ((.raw) <$> tokens)
            reconstructed === input

genTsInput :: Gen Text
genTsInput = T.concat <$> HGen.list (Range.linear 0 30) genChunk

genChunk :: Gen Text
genChunk =
    HGen.frequency
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
        HGen.list (Range.linear 0 20) $
            HGen.frequency
                [ (5, HGen.element ["foo", "bar", "Baz", ",", "\n"])
                , (1, pure "{")
                , (1, pure "}")
                , (1, genStringLiteral)
                ]
    pure $ "{" <> T.concat content <> "} from 'lib'"

genStringLiteral :: Gen Text
genStringLiteral = do
    quote <- HGen.element ["\"", "'", "`"]
    content <-
        HGen.list (Range.linear 0 10) $
            HGen.frequency
                [ (10, HGen.element ["a", "b", "c", " ", "{", "}", ";"])
                , (1, pure ("\\" <> quote)) -- Escaped quote
                ]
    pure $ quote <> T.concat content <> quote

genLineComment :: Gen Text
genLineComment = do
    content <- T.pack <$> HGen.list (Range.linear 0 20) contentChar
    pure $ "// " <> content <> "\n"

genBlockComment :: Gen Text
genBlockComment = do
    content <- T.pack <$> HGen.list (Range.linear 0 20) contentChar
    pure $ "/* " <> content <> " */"

genDocs :: Gen Text
genDocs = do
    content <- T.pack <$> HGen.list (Range.linear 0 20) contentChar
    pure $ "/** " <> content <> " */"

genWhitespace :: Gen Text
genWhitespace = T.pack <$> HGen.list (Range.linear 1 10) contentChar

genRaw :: Gen Text
genRaw = T.pack <$> HGen.list (Range.linear 1 10) contentChar

contentChar :: Gen Char
contentChar = HGen.element (['a' .. 'z'] <> ['0' .. '9'] <> ['=', '+', '-', '(', ')'])
