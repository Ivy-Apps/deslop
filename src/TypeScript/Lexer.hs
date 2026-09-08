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
        [ skipBetween (noneOf ['\n']) '"'
        , skipBetween (noneOf ['\n']) '\''
        , skipBetween anySingle '`'
        ]
  where
    skipBetween p q =
        void $ char q *> manyTill (escaped <|> void p) (char q)

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

One case is left: a quote character inside a regular expression literal
(@\/['"\`]\/@) opens a skip that is not a string. Telling a regex from a
division needs the previous token, which a scanner does not carry. See #230.
-}
pRaw :: Lexer TsToken
pRaw =
    uncurry TsToken . second (const RawK)
        <$> match (rawChunk >> manyTill rawChunk stopCondition)
  where
    rawChunk = try pSkipString <|> void anySingle
    stopCondition = lookAhead $ void atTokenStart <|> eof
    atTokenStart = choice [try $ string "//", string "/*", string "import", string "export"]
