{- | Splitting TypeScript source into the few tokens Deslop cares about.

Everything that is not an import, a re-export or a comment is one opaque
'RawK' run, so this is a scanner rather than a TypeScript parser. Rendering
the tokens back concatenates to the original source byte for byte, which is
what makes @deslop fix@ safe to run over a file it only partly understands.
-}
module TypeScript.Lexer (
    Lexer,
    lexer,
    reExportHeader,
) where

import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import TypeScript.Tokens

type Lexer = Parsec Void Text

lexer :: Lexer [TsToken]
lexer = many pToken <* eof

pToken :: Lexer TsToken
pToken =
    choice
        [ try pImport
        , try pReExport
        , try pComment
        , pWhitespace
        , pRaw
        ]

pImport :: Lexer TsToken
pImport =
    uncurry TsToken . second (const ImportK)
        <$> match (string "import" *> notFollowedBy identChar *> parseBody 0)
  where
    parseBody depth = atEnd >>= bool (body depth) (pure ())

    body d = do
        next <- lookAhead anySingle
        let d' = newDepth d next
        if d' == 0 && (next == ';' || next == '\n' || next == ')')
            then void anySingle <* optional (char ';')
            else
                choice
                    [ try pSkipString
                    , void $ try pComment
                    , void anySingle
                    ]
                    >> parseBody d'

    newDepth :: Int -> Char -> Int
    newDepth d '{' = d + 1
    newDepth d '(' = d + 1
    newDepth d '}' = max 0 (d - 1)
    newDepth d ')' = max 0 (d - 1)
    newDepth d _ = d

{- | A re-export, and only a re-export.

@export@ is orders of magnitude more common than @import@ in a TypeScript file,
and it precedes arbitrary code. Since @deslop fix@ /rewrites/ what this
classifies, the grammar is deliberately unforgiving: the shape must be
@export [type] (* [as ns] | {...}) from "specifier"@ in full, or the source
falls through to 'pRaw' untouched. @export const cfg = "./config"@ must never
become an edge, let alone get rewritten.
-}
pReExport :: Lexer TsToken
pReExport =
    uncurry TsToken . second (const ReExportK)
        <$> match reExportStatement

reExportStatement :: Lexer ()
reExportStatement = do
    quote <- reExportHeader
    void $ manyTill L.charLiteral (char quote)
    void . optional $ char ';'

{- | Everything a re-export writes before its specifier, ending on the opening
quote, which it returns. Shared with "TypeScript.Parser", which needs to know
where the specifier starts in order to rewrite it.
-}
reExportHeader :: Lexer Char
reExportHeader = do
    void $ string "export"
    notFollowedBy identChar
    pTrivia
    void . optional . try $ keyword "type"
    pTrivia
    pExportClause
    pTrivia
    keyword "from"
    pTrivia
    oneOf ['"', '\'']

{- | What may stand between @export@ and its specifier: @*@, optionally aliased,
or a braced clause. Anything else means this is not a re-export.
-}
pExportClause :: Lexer ()
pExportClause = choice [pStarClause, pNamedClause]
  where
    pStarClause = do
        void $ char '*'
        void . optional . try $ pTrivia *> keyword "as" *> pTrivia *> some identChar

    -- Braces do not nest in an export clause, but a string may appear inside
    -- one: `export { "a-b" as c } from "m"` is legal.
    pNamedClause = do
        void $ char '{'
        void . manyTill (try pSkipString <|> void anySingle) $ char '}'

-- | A bare word, checked to be a whole one rather than the head of a longer name.
keyword :: Text -> Lexer ()
keyword word = void (string word) <* notFollowedBy identChar

identChar :: Lexer Char
identChar = alphaNumChar <|> oneOf ['_', '$']

-- | Whitespace and comments, in any amount including none.
pTrivia :: Lexer ()
pTrivia = skipMany (void space1 <|> void (try pComment))

{- | A string literal, consumed whole so that nothing written inside one is
read as code.

Only a template literal may hold a raw line terminator, so the other two quotes
are bounded to the line they open on. That bound is what a stray apostrophe -
@\<p\>don't\<\/p\>@ in a @.tsx@ file, say - runs into: it fails to close, the
'try' around this backtracks, and the scanner moves on by one character instead
of swallowing everything up to the next apostrophe in the file.

The body is scanned rather than decoded, because @\\n@ written as an escape is
two characters of a perfectly ordinary single-line string.
-}
pSkipString :: Lexer ()
pSkipString =
    choice
        [ skipLine '"'
        , skipLine '\''
        , pTemplate
        ]
  where
    skipLine q = void $ char q *> manyTill (escaped <|> void (noneOf ['\n'])) (char q)

{- | A template literal and everything its interpolations hold.

Interpolations are the reason this is not a scan to the next backtick. A
template may hold @${...}@, which holds arbitrary code, which may hold another
template: closing on the first backtick found would close on the /inner opening/
one and leave the inner template's text to be read as code.

The cost is that an interpolation is skipped rather than scanned, so a dynamic
@import()@ written inside one is not an edge - a raw token is one contiguous
span, so nothing inside a region being skipped can be classified. See #231.
-}
pTemplate :: Lexer ()
pTemplate = void $ char '`' *> manyTill piece (char '`')
  where
    piece = choice [escaped, void $ try pInterpolation, void anySingle]

{- | An interpolation, up to the brace that closes it.

Braces nest and a literal may hold an unpaired one, so the closing brace is
found by counting rather than by scanning. A comment or a regular expression
holding an unpaired @}@ ends the count early - the same blind spot as #230, and
a partial exposure rather than a wrong classification.
-}
pInterpolation :: Lexer ()
pInterpolation = string "${" *> pBraced

pBraced :: Lexer ()
pBraced = void $ manyTill piece (char '}')
  where
    piece =
        choice
            [ escaped
            , try pSkipString
            , try $ char '{' *> pBraced
            , void anySingle
            ]

escaped :: Lexer ()
escaped = void $ char '\\' *> anySingle

pComment :: Lexer TsToken
pComment = try pLineComment <|> pBlockComment

pLineComment :: Lexer TsToken
pLineComment =
    uncurry TsToken . second (const CommentK)
        <$> match (string "//" *> takeWhileP (Just "comment") ('\n' /=) <* optional newline)

pBlockComment :: Lexer TsToken
pBlockComment =
    uncurry TsToken . second (const CommentK)
        <$> match (string "/*" *> manyTill anySingle (string "*/"))

pWhitespace :: Lexer TsToken
pWhitespace =
    uncurry TsToken . second (const WhitespaceK)
        <$> match (some space1)

{- | Everything Deslop does not care about, as one opaque run.

'atTokenStart' matches bare text, so a raw run has to know when it is inside a
string literal: @const banner = \`export * from "./generated";\`@ is text, and
classifying it as a re-export makes @deslop fix@ rewrite somebody's source. The
first character needs that protection as much as the rest, because after a
whitespace token a run can begin on the opening quote itself.

What is left is the regular expression literal, which 'pSkipString' does not
know about. A quote inside one (@\/['"\`]\/@) opens a skip that is not a string,
and an unpaired @}@ inside one ends an interpolation's brace count early.
Telling a regex from a division needs the previous token, which a scanner does
not carry. See #230.
-}
pRaw :: Lexer TsToken
pRaw =
    uncurry TsToken . second (const RawK)
        <$> match (rawChunk >> manyTill rawChunk stopCondition)
  where
    rawChunk =
        choice
            [ void $ takeWhile1P Nothing (not . breaksRawRun)
            , try pSkipString
            , void anySingle
            ]
    stopCondition = lookAhead $ void atTokenStart <|> eof
    atTokenStart = choice . map (try . string) $ tokenStarts

-- | What ends a raw run: a comment, or a statement this module classifies.
tokenStarts :: [Text]
tokenStarts = ["//", "/*", "import", "export"]

{- | Whether a character can end a raw run - by opening a string literal, or by
beginning one of 'tokenStarts'.

Every other character is consumed in bulk by one 'takeWhile1P', which is the
whole point: without it a raw run pays for three quote alternatives and a
four-way keyword lookahead at every character of a file that is mostly neither.

Derived from 'tokenStarts' rather than written out, because the two must agree.
A hand-written set would let a fifth keyword be added above and silently not be
looked for here, and the symptom would be a dependency that stops being found.
-}
breaksRawRun :: Char -> Bool
breaksRawRun = (`elem` rawRunBreaks)

rawRunBreaks :: [Char]
rawRunBreaks = ['"', '\'', '`'] <> mapMaybe (fmap fst . T.uncons) tokenStarts
