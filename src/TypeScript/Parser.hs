module TypeScript.Parser (
    parseTs,
    parseImport,
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
        p1 <- T.pack <$> manyTill anySingle (string "from")
        p2 <- string "from"
        p3 <- mconcat <$> some (string " ")
        p4 <- pQuote
        t <- manyTill anySingle (char p4)
        s <- takeRest
        pure
            Import
                { prefix = p1 <> p2 <> p3 <> T.singleton p4
                , target = T.pack t
                , suffix = s
                }

pQuote :: Parser Char
pQuote = char '\'' <|> char '"'
