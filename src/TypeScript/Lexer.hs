module TypeScript.Lexer where

import Data.Text
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
pImport = ImportTok . fst <$> match (string "import" *> manyTill anySingle end)
  where
    end = choice [try $ string ";\n", string ";", string "\n", string ")"]

pComment :: Lexer TsToken
pComment = pLineComment

pLineComment :: Lexer TsToken
pLineComment = do
    (raw, comment) <-
        match $
            string "//"
                *> optional (char ' ')
                *> takeWhileP (Just "comment content") ((/=) '\n')
                <* optional newline
    return CommentTok {comment = comment, raw = raw}

pRaw :: Lexer TsToken
pRaw = do
    (raw, _) <- match (anySingle >> manyTill anySingle (lookAhead (atTokenStart <|> eof)))
    return RawTok {raw = raw}
  where
    atTokenStart = () <$ string "//" <|> () <$ string "import"