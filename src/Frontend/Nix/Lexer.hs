{- | Splitting Nix source into 'Lexeme's, losslessly, following the rules of
Nix's own lexer (@src\/libexpr\/lexer.l@) wherever they decide what is a path.

What decides it is lexical, so it is followed exactly: a path is a run of
path characters holding at least one slash followed by more of them, so
@.\/a@, @a\/b@ and @\/etc@ are paths and @a \/ b@ and @a\/\/b@ are not. A
@#@ or @\/* *\/@ comment, a @"..."@ string and an @''...''@ string are each
one lexeme, so nothing inside one is mistaken for a path, while the code in a
string's or a path's @${ }@ is lexed as code, so a path inside one is found.
Where two rules could claim the same text, the longer wins, as in Nix: @x:y@
is a URI.

Total in a weaker sense than the Haskell lexer: text Nix itself could not
lex - an unterminated string or comment, an interpolation never closed, a path
ending in a slash - is 'Nothing', and "Frontend.Nix.Unit" keeps such a file as
one opaque island.
-}
module Frontend.Nix.Lexer (
    lexNix,
    isIdentifier,
    PathLiteralSpan (..),
    pathLiteralSpans,
) where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text qualified as T
import Frontend.Nix.Lexeme (Lexeme (..), Part (..), Quoting (..), interpolations, nesting)
import Renderable qualified
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)

type Lexer = Parsec Void Text

{- | Every lexeme of the source, in order, or 'Nothing' for text Nix could not
lex either. @foldMap render \<$\> lexNix t@ is @Just t@ whenever it is not
'Nothing'.
-}
lexNix :: Text -> Maybe [Lexeme]
lexNix = mfilter (not . hasTrailingSlash) . rightToMaybe . runParser (many codeLexeme <* eof) ""

{- | Whether a path is followed by a slash, at the top or in an interpolation.
A path ends before a slash with nothing after it, so the slash starts the
next lexeme - @\/@, @\/\/@, @\/* *\/@ - and the whole is text Nix rejects:
@path has a trailing slash@. Asked of the lexemes, not while lexing,
because a path inside an interpolation inside a path is lexed under a
backtracking parser, which would quietly read the text some other way.
-}
hasTrailingSlash :: [Lexeme] -> Bool
hasTrailingSlash lexemes =
    any slashAfterPath (zip lexemes (drop 1 lexemes))
        || any (any (hasTrailingSlash . snd) . interpolations 0) lexemes
  where
    slashAfterPath = \case
        (PathLit _, next) -> "/" `T.isPrefixOf` Renderable.render next
        _ -> False

{- | One path literal of a file, where its bytes are: from @start@, inclusive,
to @end@, exclusive, counted in bytes of UTF-8 as @nix-instantiate@ counts.
-}
data PathLiteralSpan = PathLiteralSpan
    { start :: Int
    , end :: Int
    , literal :: Text
    -- ^ As written, interpolations included.
    }
    deriving stock (Show, Eq)

{- | Every path literal in the source, in order, including those inside an
interpolation, and none inside a comment or a string's own text. 'Nothing'
when the source does not lex. What a corpus judges this lexer by against
@nix-instantiate --parse@.
-}
pathLiteralSpans :: Text -> Maybe [PathLiteralSpan]
pathLiteralSpans source = spansIn 0 <$> lexNix source
  where
    spansIn offset = concat . snd . mapAccumL (\o l -> (o + T.length (Renderable.render l), spansOf o l)) offset
    spansOf o l = [spanAt o (Renderable.render l) | PathLit _ <- [l]] <> concatMap (uncurry spansIn) (interpolations o l)
    spanAt o literal =
        let start = bytesBefore o
         in PathLiteralSpan {start, end = start + bytesOf literal, literal}
    bytesBefore = bytesOf . (`T.take` source)
    bytesOf = BS.length . encodeUtf8

-- | One lexeme of code, at the top of a file or inside an interpolation.
codeLexeme :: Lexer Lexeme
codeLexeme =
    choice
        [ Trivia <$> takeWhile1P Nothing isSpace
        , Trivia <$> lineComment
        , Trivia <$> blockComment
        , indentedString
        , doubleQuotedString
        , try uri
        , try pathLiteral
        , try searchPath
        , Identifier <$> identifier
        , Other <$> number
        , Punctuation <$> punctuation
        , Other . T.singleton <$> anySingle
        ]

lineComment :: Lexer Text
lineComment = fmap fst . match $ char '#' *> takeWhileP Nothing (/= '\n')

-- | Not nested, as in Nix: the first @*\/@ closes it.
blockComment :: Lexer Text
blockComment = fmap fst . match $ string "/*" *> manyTill anySingle (string "*/")

doubleQuotedString :: Lexer Lexeme
doubleQuotedString = Str DoubleQuoted <$> (char '"' *> many part <* char '"')
  where
    part = interpolation <|> chunkOf (some piece)
    piece =
        choice
            [ void (string "$$")
            , char '\\' *> void anySingle
            , try $ char '$' *> notFollowedBy (char '{')
            , void $ satisfy (`notElem` ("\"\\$" :: String))
            ]

-- | Ends at the first @''@ that does not escape what follows it.
indentedString :: Lexer Lexeme
indentedString = Str Indented <$> (string "''" *> many part <* string "''")
  where
    part = interpolation <|> chunkOf (some piece)
    piece =
        choice
            [ void . try $ string "'''"
            , void . try $ string "''$"
            , try $ string "''\\" *> void anySingle
            , void (string "$$")
            , try $ char '$' *> notFollowedBy (char '{')
            , try $ char '\'' *> notFollowedBy (char '\'')
            , void $ satisfy (`notElem` ("$'" :: String))
            ]

chunkOf :: Lexer a -> Lexer Part
chunkOf = fmap (Chunk . fst) . match

{- | @${@, the code inside and the @}@ closing it: the first one no @{@ or
@${@ inside the code opened.
-}
interpolation :: Lexer Part
interpolation = string "${" *> (Interpolation <$> code (0 :: Int))
  where
    code depth = (guard (depth == 0) *> char '}' $> []) <|> next depth
    next depth = do
        lexeme <- codeLexeme
        (lexeme :) <$> code (depth + nesting lexeme)

{- | Path characters, then at least one slash followed by more of them or by
an interpolation. A home path starts with @~@ instead. A trailing slash is
not part of the path; Nix rejects it, and so does 'lexNix'.
-}
pathLiteral :: Lexer Lexeme
pathLiteral = do
    prefix <- chunkOf (void (char '~')) <|> chunkOf (takeWhileP Nothing isPathChar)
    segments <- some segment
    pure . PathLit . mergeChunks $ prefix : concat segments
  where
    segment = try $ (:) <$> chunkOf (char '/') <*> some (interpolation <|> chunkOf (takeWhile1P Nothing isPathChar))

-- | Adjacent chunks as one, so a path is never split where nothing is interpolated.
mergeChunks :: [Part] -> [Part]
mergeChunks = \case
    Chunk a : Chunk b : rest -> mergeChunks (Chunk (a <> b) : rest)
    p : rest -> p : mergeChunks rest
    [] -> []

isPathChar :: Char -> Bool
isPathChar c = isAsciiAlphaNum c || c `elem` ("._-+" :: String)

-- | @<nixpkgs>@, @<nixpkgs/lib>@.
searchPath :: Lexer Lexeme
searchPath = do
    _ <- char '<'
    (inner, _) <- match $ takeWhile1P Nothing isPathChar *> many (char '/' *> takeWhile1P Nothing isPathChar)
    SearchPath inner <$ char '>'

uri :: Lexer Lexeme
uri = fmap (Uri . fst) . match $ do
    _ <- satisfy isAsciiAlpha
    _ <- takeWhileP Nothing (\c -> isAsciiAlphaNum c || c `elem` ("+-." :: String))
    _ <- char ':'
    takeWhile1P Nothing (\c -> isAsciiAlphaNum c || c `elem` ("%/?:@&=+$,-_.!~*'" :: String))

identifier :: Lexer Text
identifier = fmap fst . match $ satisfy isIdentifierStart *> takeWhileP Nothing isIdentifierChar

-- | Whether the text is one identifier, such as a name a value can be bound to.
isIdentifier :: Text -> Bool
isIdentifier name = case T.uncons name of
    Just (c, rest) -> isIdentifierStart c && T.all isIdentifierChar rest
    Nothing -> False

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAsciiAlpha c || c == '_'

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAsciiAlphaNum c || c `elem` ("_'-" :: String)

number :: Lexer Text
number = fmap fst . match $ takeWhile1P Nothing isDigit *> optional (try (char '.' *> takeWhile1P Nothing isDigit))

-- | The longest operator first, so @//@ is one lexeme and not two slashes.
punctuation :: Lexer Text
punctuation =
    choice (fmap (try . string) ["...", "${", "==", "!=", "<=", ">=", "&&", "||", "->", "//", "++", "|>", "<|"])
        <|> T.singleton
        <$> satisfy (`elem` ("{}[]();:,.=@?!+-*/<>" :: String))

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isDigit c
