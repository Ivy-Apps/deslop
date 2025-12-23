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
        , try pComment
        , pRaw
        ]

pImport :: Lexer TsToken
pImport = do
    (raw, _) <- match (string "import" *> manyTill anySingle end)
    return $ TsToken raw ImportK
  where
    end = choice [try $ string ";\n", string ";", string "\n", string ")"]

pComment :: Lexer TsToken
pComment = try pLineComment <|> pBlockComment

pLineComment :: Lexer TsToken
pLineComment =
    uncurry TsToken . second (CommentK . T.strip)
        <$> match
            ( string "//"
                *> takeWhileP (Just "comment") ((/=) '\n')
                <* optional newline
            )

pBlockComment :: Lexer TsToken
pBlockComment =
    uncurry TsToken . second (CommentK . T.strip . T.pack)
        <$> match (string "/*" *> manyTill anySingle (string "*/"))

pRaw :: Lexer TsToken
pRaw = do
    (raw, _) <- match (anySingle >> matchTillToken)
    return $ TsToken raw RawK
  where
    matchTillToken = manyTill anySingle (lookAhead $ () <$ atTokenStart <|> eof)
    atTokenStart = choice [try $ string "//", string "/*", string "import"]