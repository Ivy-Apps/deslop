module TypeScript.Parser (
    TsFile (..),
    parseTs,
    renderAst
) where

import Data.Bifunctor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import TypeScript.AST
import TypeScript.Lexer
import TypeScript.Tokens
import Data.List (foldl')

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
    parser :: Parser TsNode
    parser = do
        (p1, q) <- manyTill_ anySingle pQuote
        t <- takeWhile1P (Just "target") (/= q)
        s <- takeRest
        pure
            Import
                { prefix = T.pack p1 <> T.singleton q
                , target = t
                , suffix = s
                }

pQuote :: Parser Char
pQuote = char '\'' <|> char '"'

renderAst :: [TsNode] -> Text
renderAst = foldl' combine ""
  where
    combine :: Text -> TsNode -> Text
    combine r n = r <> render n

    render :: TsNode -> Text
    render (Source r) = r
    render (Docs r _) = r
    render (Comment r _) = r
    render (Import p t s) = p <> t <> s