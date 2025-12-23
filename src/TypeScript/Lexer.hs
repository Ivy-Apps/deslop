module TypeScript.Lexer where

import Data.Text
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
pComment = pLineComment

pLineComment :: Lexer TsToken
pLineComment = do
    (raw, comment) <-
        match $
            string "//"
                *> optional (char ' ')
                *> takeWhileP (Just "comment content") ((/=) '\n')
                <* optional newline
    return $ TsToken raw CommentK {comment = comment}

pRaw :: Lexer TsToken
pRaw = do
    (raw, _) <- match (anySingle >> matchTillToken)
    return $ TsToken raw RawK
  where
    matchTillToken = manyTill anySingle (lookAhead $ () <$ atTokenStart <|> eof)
    atTokenStart = choice [string "//", string "import"]