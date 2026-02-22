module TypeScript.Parser (
    TsFile (..),
    parseTs,
) where

import Data.Bifunctor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import TypeScript.AST
import TypeScript.Lexer
import TypeScript.Tokens

type Parser = Parsec Void Text

data TsFile = TsFile
    { path :: FilePath
    , content :: Text
    }
    deriving (Show, Eq)

parseTs :: TsFile -> Either String TsProgram
parseTs f =
    first errorBundlePretty $
        TsModule f.path . buildAST <$> runParser lexer f.path f.content

buildAST :: [TsToken] -> [TsNode]
buildAST = fmap node
  where
    node :: TsToken -> TsNode
    node (TsToken r RawK) = Source r
    node (TsToken r WhitespaceK) = Source r
    node (TsToken r (CommentK c)) = Comment {raw = r, content = c}
    node (TsToken r (DocsK c)) = Docs {raw = r, content = c}
    node (TsToken r ImportK) = case parseImport r of
        Left _ -> Source r
        Right n -> n

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

