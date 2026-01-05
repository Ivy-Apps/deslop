module TypeScript.Lexer where

import Data.Bifunctor (second)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import TypeScript.Tokens
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

lexer :: Lexer [TsToken]
lexer = many pToken <* eof

pToken :: Lexer TsToken
pToken =
    choice
        [ try pImport
        , try pDocs
        , try pComment
        , pWhitespace
        , pRaw
        ]

pImport :: Lexer TsToken
pImport =
    uncurry TsToken . second (const ImportK)
        <$> match (string "import" *> parseBody 0)
  where
    parseBody depth = do
        done <- atEnd
        if done 
            then pure () 
            else do
                next <- lookAhead anySingle
                if depth == 0 && (next == ';' || next == '\n')
                    then void anySingle
                    else do
                        choice
                            [ try pSkipString
                            , void $ try pComment
                            , void anySingle
                            ] >> parseBody (currentDepth depth next)

    currentDepth :: Int -> Char -> Int
    currentDepth d '{' = d + 1
    currentDepth d '(' = d + 1
    currentDepth d '}' = max 0 (d - 1)
    currentDepth d ')' = max 0 (d - 1)
    currentDepth d _   = d

    pSkipString = choice 
        [ skipBetween '"'
        , skipBetween '\'' 
        , skipBetween '`'
        ]
      where 
        skipBetween q = 
            char q >> manyTill L.charLiteral (char q) >> pure ()

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

pWhitespace :: Lexer TsToken
pWhitespace =
    uncurry TsToken . second (const WhitespaceK)
        <$> match (some space1)

pRaw :: Lexer TsToken
pRaw =
    uncurry TsToken . second (const RawK)
        <$> match (anySingle >> manyTill anySingle stopCondition)
  where
    stopCondition = lookAhead $ () <$ atTokenStart <|> eof
    atTokenStart = choice [try $ string "//", string "/*", string "import"]