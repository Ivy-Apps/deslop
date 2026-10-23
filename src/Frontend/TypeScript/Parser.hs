module Frontend.TypeScript.Parser (
    TsFile (..),
    parseTs,
) where

import Data.Text qualified as T
import FileSystem.Path (AbsPath (..))
import Frontend.TypeScript.CST
import Frontend.TypeScript.Lexer
import Frontend.TypeScript.Tokens
import Text.Megaparsec

type Parser = Parsec Void Text

data TsFile = TsFile
    { path :: AbsPath
    , content :: Text
    }
    deriving (Show, Eq)

{- | Total. The lexer is an island grammar: what it does not recognise is a raw
token, so it reads any text. Were it ever to fail, the file would be one raw
island, which still renders back to itself.
-}
parseTs :: TsFile -> TsProgram
parseTs f = TsModule f.path . either (const [Source f.content]) buildCST $ runParser lexer "" f.content

buildCST :: [TsToken] -> [TsNode]
buildCST = fmap node
  where
    node :: TsToken -> TsNode
    node (TsToken r RawK) = Source r
    node (TsToken r WhitespaceK) = Source r
    node (TsToken r CommentK) = Source r
    node (TsToken r ImportK) = either (const $ Source r) identity (parseImport r)
    node (TsToken r ReExportK) = either (const $ Source r) identity (parseReExport r)

parseImport :: Text -> Either String TsNode
parseImport = first errorBundlePretty . runParser parser ""
  where
    -- import type { User, Permissions } from "./types";
    -- import { "hello" as hell } from '@/lib/bug'
    parser :: Parser TsNode
    parser = do
        (p, q) <- consumeUntilTarget 0 ""
        t <- takeWhile1P (Just "target") (/= q)
        s <- takeRest
        pure
            Import
                { prefix = p <> T.singleton q
                , target = t
                , suffix = s
                }

    consumeUntilTarget :: Int -> Text -> Parser (Text, Char)
    consumeUntilTarget d acc = do
        next <- anySingle
        let d' = newDepth d next
        case targetQuote d' next of
            Just q -> pure (acc, q)
            Nothing -> consumeUntilTarget d' (acc <> T.singleton next)

    newDepth :: Int -> Char -> Int
    newDepth d '{' = d + 1
    newDepth d '}' = d - 1
    newDepth d _ = d

    targetQuote :: Int -> Char -> Maybe Char
    targetQuote 0 c
        | c == '\'' || c == '"' = Just c
        | otherwise = Nothing
    targetQuote _ _ = Nothing

{- | Re-runs the lexer's own grammar to find where the specifier starts, so
that the two can never disagree about what a re-export is.
-}
parseReExport :: Text -> Either String TsNode
parseReExport = first errorBundlePretty . runParser parser ""
  where
    parser :: Parser TsNode
    parser = do
        (p, q) <- match reExportHeader
        t <- takeWhile1P (Just "target") (/= q)
        s <- takeRest
        pure
            ReExport
                { prefix = p
                , target = t
                , suffix = s
                }
