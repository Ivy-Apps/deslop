module TypeScript.Lexer where

import Data.Bifunctor (second)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import TypeScript.Tokens

type Lexer = Parsec Void Text

lexer :: Lexer [TsToken]
lexer = many pToken <* eof

pToken :: Lexer TsToken
pToken =
    choice
        [ try pImport
        , try pDocs
        , try pComment
        , pRaw
        ]

pImport :: Lexer TsToken
pImport =
    uncurry TsToken . second (const ImportK)
        <$> match (string "import" *> manyTill anySingle end)
  where
    end = choice [try $ string ";\n", string ";", string "\n", string ")"]

pDocs :: Lexer TsToken
pDocs =
    uncurry TsToken . second (DocsK . T.strip . T.pack)
        <$> match (string "/**" *> manyTill anySingle (string "*/"))

pComment :: Lexer TsToken
pComment = try pLineComment <|> pBlockComment

pLineComment :: Lexer TsToken
pLineComment =
    uncurry TsToken . second (CommentK . T.strip)
        <$> match (string "//" *> takeWhileP (Just "comment") ((/=) '\n') <* optional newline)

pBlockComment :: Lexer TsToken
pBlockComment =
    uncurry TsToken . second (CommentK . T.strip . T.pack)
        <$> match (string "/*" *> manyTill anySingle (string "*/"))

pRaw :: Lexer TsToken
pRaw =
    uncurry TsToken . second (const RawK)
        <$> match (anySingle >> manyTill anySingle stopCondition)
  where
    stopCondition = lookAhead $ () <$ atTokenStart <|> eof
    atTokenStart = choice [try $ string "//", string "/*", string "import"]