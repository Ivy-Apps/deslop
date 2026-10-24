{- | Reading a module header and its imports out of Haskell tokens.

Layout is followed the way the Haskell report defines it for the top level of
a module. After @where@ - or from the first token, in a file with no header -
either an explicit @{@ opens a block whose declarations are separated by @;@
until the matching @}@, or the column of the first token is the layout
column, and every token that starts at or left of it begins the next
declaration. An explicit @;@ separates declarations in both. The import section
is every declaration up to the first that does not start with @import@.
-}
module Frontend.Haskell.Parser (parseHaskell) where

import Data.List qualified as L
import Data.Text qualified as T
import Frontend.Haskell.CST (Header (..), HsSource (..), Import (..), ImportDecl (..), ModuleExport (..), ModuleHeader (..), Qualification (..))
import Frontend.Haskell.ModuleName (HsModuleName, fromDotted)
import Frontend.Haskell.Tokens (HsToken (..), HsTokenKind (..), Positioned (..), isKeyword, isTrivia, positioned)
import Renderable (Renderable (..))
import Text.Megaparsec (Parsec, anySingle, eof, runParser, satisfy, token)

{- | The source's header and imports, or 'Nothing' when it has a @module@ line
that never reaches @where@, or an import that is not one: a file GHC would
reject, whose imports Deslop will not guess at. Lossless: rendering a parsed
source gives back the tokens it was parsed from.
-}
parseHaskell :: [HsToken] -> Maybe HsSource
parseHaskell = parseTokens . positioned

-- | How the top-level declarations of a module are delimited.
data Layout
    = Braces
    | Column Int

parseTokens :: [Positioned] -> Maybe HsSource
parseTokens toks = do
    let (leading, afterLeading) = span trivia toks
    (header, afterHeader) <- headerOf afterLeading
    let (opening, layout, declarations) = openBlock afterHeader
    (imports, body) <- importSection layout declarations
    pure
        HsSource
            { leading = tokensOf leading
            , header = header
            , opening = tokensOf opening
            , imports = imports
            , body = tokensOf body
            }

headerOf :: [Positioned] -> Maybe (Header, [Positioned])
headerOf = \case
    toks@(lead : _)
        | isKeyword "module" lead.token -> do
            (headerToks, rest) <- throughWhere toks
            h <- moduleHeader headerToks
            pure (Declared h, rest)
    toks -> Just (ImplicitMain, toks)

-- | Up to and including the first @where@ outside any brackets.
throughWhere :: [Positioned] -> Maybe ([Positioned], [Positioned])
throughWhere toks = case break (isKeyword "where" . (.token) . snd) (zip (depths toks) toks) of
    (before, (0, wherePos) : after) -> Just (fmap snd before <> [wherePos], fmap snd after)
    _ -> Nothing

moduleHeader :: [Positioned] -> Maybe ModuleHeader
moduleHeader toks = case filter (not . trivia) toks of
    _module : nameTok : rest -> do
        name <- moduleNameOf nameTok.token
        reExports <- exportsOf rest
        pure ModuleHeader {name, reExports, tokens = tokensOf toks}
    _ -> Nothing
  where
    -- Between the name and @where@: nothing, or a parenthesised export list.
    -- A @WARNING@ or @DEPRECATED@ pragma there is trivia, filtered out above.
    exportsOf = \case
        [_where] -> Just []
        open : more | isSpecial "(" open.token -> case reverse more of
            _where : close : _ | isSpecial ")" close.token -> Just . moduleExports $ insideParens toks
            _ -> Nothing
        _ -> Nothing

-- | The tokens strictly between the export list's outer parentheses.
insideParens :: [Positioned] -> [Positioned]
insideParens toks =
    let afterOpen = drop 1 . dropWhile (not . isSpecial "(" . (.token)) $ toks
        closes (depth, p) = depth == 0 && isSpecial ")" p.token
     in fmap snd . takeWhile (not . closes) $ zip (depths afterOpen) afterOpen

-- | @module X@ items of an export list, which is split at its top-level commas.
moduleExports :: [Positioned] -> [ModuleExport]
moduleExports = mapMaybe moduleExport . splitTopLevel (isSpecial ",")
  where
    moduleExport item = case filter (not . trivia) item of
        [moduleKw, nameTok] | isKeyword "module" moduleKw.token -> do
            name <- moduleNameOf nameTok.token
            pure ModuleExport {name, line = moduleKw.line, code = render . tokensOf . trimTrivia $ item}
        _ -> Nothing

-- | After the header: an explicit @{@, or the column of the first declaration.
openBlock :: [Positioned] -> ([Positioned], Layout, [Positioned])
openBlock toks = case span trivia toks of
    (gap, brace : more)
        | isSpecial "{" brace.token ->
            let (gap', declarations) = span trivia more
             in (gap <> [brace] <> gap', Braces, declarations)
    (gap, declarations@(lead : _)) -> (gap, Column lead.column, declarations)
    (gap, []) -> (gap, Column 1, [])

{- | The imports, then the body: everything from the lead declaration that is
not an import. 'Nothing' when a declaration starting with @import@ is not one.
-}
importSection :: Layout -> [Positioned] -> Maybe ([Import], [Positioned])
importSection layout toks = case nextDeclaration layout toks of
    Just (own@(lead : _), trailing, rest)
        | isKeyword "import" lead.token -> do
            decl <- importDecl own
            (more, body) <- importSection layout rest
            pure (Import {decl, line = lead.line, own = tokensOf own, trailing = tokensOf trailing} : more, body)
    _ -> Just ([], toks)

{- | One declaration: its own tokens, the trivia and separators after it, and
what follows. 'Nothing' at the end of the block.
-}
nextDeclaration :: Layout -> [Positioned] -> Maybe ([Positioned], [Positioned], [Positioned])
nextDeclaration layout = \case
    [] -> Nothing
    lead : _ | Braces <- layout, isSpecial "}" lead.token -> Nothing
    lead : more ->
        let (inside, after) = break ends (zip (depths more) more)
            (own, gap) = splitTrailingTrivia (lead : fmap snd inside)
            (separators, rest) = span (\p -> trivia p || isSpecial ";" p.token) (fmap snd after)
         in Just (own, gap <> separators, rest)
  where
    ends (depth, p) = case layout of
        Braces -> depth == 0 && (isSpecial ";" p.token || isSpecial "}" p.token)
        Column n -> (depth == 0 && isSpecial ";" p.token) || (not (trivia p) && p.column <= n)

-- | The bracket depth each token sits at, counted from the lead.
depths :: [Positioned] -> [Int]
depths = L.scanl' (\depth t -> max 0 (depth + bracketDelta t)) 0 . fmap (.token)

splitTopLevel :: (HsToken -> Bool) -> [Positioned] -> [[Positioned]]
splitTopLevel isSeparator toks = go (zip (depths toks) toks)
  where
    go xs = case break (\(depth, p) -> depth == 0 && isSeparator p.token) xs of
        (item, []) -> [fmap snd item]
        (item, _ : rest) -> fmap snd item : go rest

splitTrailingTrivia :: [Positioned] -> ([Positioned], [Positioned])
splitTrailingTrivia toks =
    let (gap, own) = span trivia (reverse toks)
     in (reverse own, reverse gap)

trimTrivia :: [Positioned] -> [Positioned]
trimTrivia = fst . splitTrailingTrivia . dropWhile trivia

--------------------------------------------------------------------------------
-- One import declaration
--------------------------------------------------------------------------------

type TokenParser = Parsec Void [HsToken]

{- | @import [safe] [qualified] ["pkg"] M [qualified] [as N] [hiding] [(...)]@.
A @{\-# SOURCE #-\}@ pragma is trivia here: a source import reaches the same
module.
-}
importDecl :: [Positioned] -> Maybe ImportDecl
importDecl = rightToMaybe . runParser grammar "" . filter (not . isTrivia) . tokensOf
  where
    grammar :: TokenParser ImportDecl
    grammar = do
        keyword "import"
        skip $ keyword "safe"
        before <- optional (keyword "qualified")
        package <- optional packageName
        moduleName <- modName
        after <- optional (keyword "qualified")
        let qualification = bool Unqualified Qualified (isJust (before <|> after))
        alias <- optional (keyword "as" *> modName)
        skip $ keyword "hiding"
        skip importList
        eof
        pure ImportDecl {package, moduleName, qualification, alias}

    skip = void . optional
    packageName = token (\t -> guard (t.kind == StringK) $> (T.dropEnd 1 . T.drop 1 $ t.raw)) mempty
    importList = special "(" *> nested 1

    -- Balanced brackets, with whatever they hold: @type@, @pattern@, operators.
    nested :: Int -> TokenParser ()
    nested 0 = pure ()
    nested depth = do
        t <- anySingle
        nested (depth + bracketDelta t)

keyword :: Text -> TokenParser ()
keyword word = void $ satisfy (isKeyword word)

special :: Text -> TokenParser ()
special s = void $ satisfy (isSpecial s)

modName :: TokenParser HsModuleName
modName = token moduleNameOf mempty

moduleNameOf :: HsToken -> Maybe HsModuleName
moduleNameOf t = guard (t.kind == ConIdK) *> fromDotted t.raw

isSpecial :: Text -> HsToken -> Bool
isSpecial s t = t.kind == SpecialK && t.raw == s

-- | How far a token moves the bracket depth: in by an opening one, out by a closing one.
bracketDelta :: HsToken -> Int
bracketDelta t
    | any (`isSpecial` t) ["(", "[", "{"] = 1
    | any (`isSpecial` t) [")", "]", "}"] = -1
    | otherwise = 0

trivia :: Positioned -> Bool
trivia = isTrivia . (.token)

tokensOf :: [Positioned] -> [HsToken]
tokensOf = fmap (.token)
