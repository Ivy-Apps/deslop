module TypeScript.Lexer where

import Data.Text
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
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
  start <- string "import"
  (content, end) <- manyTill_ anySingle pImportEnd
  let raw = start <> T.pack content <> T.singleton end
  return ImportTok {raw = raw}
 where
  pImportEnd = choice [char ';', char '\n', char ')']

pComment :: Lexer TsToken
pComment = fail "WIP"

pLineComment :: Lexer TsToken
pLineComment = do
  prefix <- string "//"
  comment <- takeWhileP (Just "comment content") ((/=) '\n')
  return CommentTok {comment = comment, raw = prefix <> comment <> T.singleton '\n'}

pRaw :: Lexer TsToken
pRaw = do
  c <- anySingle
  rest <- takeWhileP Nothing (not . knownStart)
  return RawTok {raw = T.singleton c <> rest}
 where
  knownStart :: Char -> Bool
  knownStart '/' = True
  knownStart 'i' = True
  knownStart _ = False