{- | Splitting Haskell source into 'HsToken's, losslessly.

The rules that decide whether an @import@ is really one are lexical, so they
are the ones followed exactly: block comments nest, @{\-# ... #-\}@ is a pragma, and a
run of symbol characters is a line comment only when it is all dashes and at
least two long - @-- |@ is a comment, @--|@ and @-->@ are operators. Strings
and character literals are tokens of their own, so nothing inside one is ever
read as a comment or a keyword.

Total: a character no rule claims is an 'OtherK' token, and an unterminated
comment or string runs to where GHC would stop reading it.
-}
module Frontend.Haskell.Lexer (lexHaskell, isIdentChar) where

import Data.Char (isAlphaNum, isAscii, isLower, isPunctuation, isSpace, isSymbol, isUpper)
import Data.Text qualified as T
import Frontend.Haskell.Tokens (HsToken (..), HsTokenKind (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, string)

type Lexer = Parsec Void Text

{- | Every token of the source, in order. @mconcat (fmap raw (lexHaskell t)) == t@
for every @t@.
-}
lexHaskell :: Text -> [HsToken]
lexHaskell source = fromRight [HsToken source OtherK | not (T.null source)] $ runParser allTokens "" source
  where
    allTokens = (<>) . maybeToList <$> optional shebang <*> many hsToken <* eof

hsToken :: Lexer HsToken
hsToken =
    choice
        [ lexeme WhitespaceK $ takeWhile1P Nothing isWhite
        , lexeme PragmaK pragma
        , lexeme BlockCommentK blockComment
        , symbolRun
        , lexeme StringK stringLiteral
        , try $ lexeme CharK charLiteral
        , lexeme VarIdK varId
        , lexeme ConIdK conIds
        , lexeme SpecialK . void $ satisfy (`elem` specials)
        , lexeme OtherK . void $ anySingle
        ]

lexeme :: HsTokenKind -> Lexer a -> Lexer HsToken
lexeme kind = fmap ((`HsToken` kind) . fst) . match

-- | GHC skips a @#!@ line that opens the file, so a script can be run directly.
shebang :: Lexer HsToken
shebang = lexeme LineCommentK $ string "#!" *> takeWhileP Nothing (/= '\n')

-- | A byte-order mark is whitespace wherever it is; GHC skips one opening a file.
isWhite :: Char -> Bool
isWhite c = isSpace c || c == '\xFEFF'

pragma :: Lexer ()
pragma = string "{-#" *> untilOrEnd (string "#-}")

blockComment :: Lexer ()
blockComment = string "{-" *> rest
  where
    rest =
        choice
            [ void $ string "-}"
            , blockComment *> rest
            , anySingle *> rest
            , eof
            ]

-- | Everything up to and including the terminator, or to the end of input.
untilOrEnd :: Lexer Text -> Lexer ()
untilOrEnd terminator = void (try terminator) <|> eof <|> (anySingle *> untilOrEnd terminator)

-- | The dash rule: all dashes, two or more, is a comment to the end of the line.
symbolRun :: Lexer HsToken
symbolRun = do
    run <- takeWhile1P Nothing isSymbolChar
    if T.length run >= 2 && T.all (== '-') run
        then (`HsToken` LineCommentK) . (run <>) <$> takeWhileP Nothing (/= '\n')
        else pure (HsToken run SymbolK)

isSymbolChar :: Char -> Bool
isSymbolChar c
    | isAscii c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
    | otherwise = (isSymbol c || isPunctuation c) && c /= '\xFEFF'

specials :: String
specials = "(),;[]`{}"

-- | Closed at the matching quote, or left open at the end of the line.
stringLiteral :: Lexer ()
stringLiteral = char '"' *> many piece *> void (optional (char '"'))
  where
    piece =
        choice
            [ try $ char '\\' *> takeWhile1P Nothing isSpace *> void (char '\\')
            , -- Never fails having consumed the backslash: a backslash that
              -- ends the line ends the string, and the lexer goes on.
              try $ char '\\' *> void (satisfy (/= '\n'))
            , void $ satisfy (`notElem` ("\"\n\\" :: String))
            ]

{- | @'a'@, @'\\n'@, @'\\SOH'@. A quote that opens none - a promoted
constructor, a Template Haskell name quote - is left to 'OtherK'.
-}
charLiteral :: Lexer ()
charLiteral = char '\'' *> (escaped <|> plain) *> void (char '\'')
  where
    escaped = char '\\' *> (void (takeWhile1P Nothing isAlphaNum) <|> void (satisfy (/= '\n')))
    plain = void $ satisfy (`notElem` ("'\n" :: String))

varId :: Lexer ()
varId = satisfy (\c -> isLower c || c == '_') *> void (takeWhileP Nothing isIdentChar)

-- | @Data.Map.Strict@ is one token: a module name is what an import names.
conIds :: Lexer ()
conIds = conId *> many (try (char '.' *> conId)) $> ()
  where
    conId = satisfy isUpper *> takeWhileP Nothing isIdentChar

-- | What may follow the first letter of an identifier or a module name segment.
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''
