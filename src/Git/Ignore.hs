{-# LANGUAGE QuasiQuotes #-}

{- | A self-contained @.gitignore@ implementation, following gitignore(5).

The matcher is a hand-rolled wildmatch, the same algorithm git itself uses in
@wildmatch.c@: a pattern compiles to path /segments/, each segment to /tokens/,
and matching walks both in lockstep with backtracking on @**@. Deliberately no
regex engine, so compiled patterns derive 'Show' and 'Eq' and stay inspectable.

Only committed, in-tree @.gitignore@ files are consulted; @.git/info/exclude@
and @core.excludesFile@ are not, so a given commit yields the same verdict on
CI and on every developer machine. See @docs/adr/0004@.
-}
module Git.Ignore (
    -- * Model
    GitIgnore (..),
    IgnoreScope (..),
    IgnoreRule (..),
    IgnorePattern (..),
    Seg (..),
    Tok (..),
    CharMatch (..),
    ClassItem (..),
    PosixClass (..),

    -- * Loading
    loadGitIgnore,
    emptyGitIgnore,

    -- * Parsing
    parseIgnoreFile,
    parseIgnoreRule,
    renderIgnoreRule,

    -- * Matching
    isIgnored,
    alwaysIgnored,
    alwaysIgnoredNames,
) where

import Data.Char (isAlpha, isAlphaNum, isControl, isDigit, isHexDigit, isLower, isPrint, isPunctuation, isSeparator, isSpace, isSymbol, isUpper)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe, decodeOsPath, encodeOsPath, fsReadFile, withAbsBaseUnsafe)
import FileSystem.Iterator (Entry (..), walkDir)
import System.OsPath (OsPath, osp, splitDirectories, takeDirectory, takeFileName)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

-- | Matches exactly one character.
data CharMatch
    = Lit !Char
    | -- | @?@
      AnyChar
    | -- | @[a-z]@, or @[!a-z]@ when the flag is set
      Class !Bool ![ClassItem]
    deriving (Show, Eq)

data ClassItem
    = ClassChar !Char
    | ClassRange !Char !Char
    | -- | @[:alpha:]@ and friends
      ClassPosix !PosixClass
    deriving (Show, Eq)

data PosixClass
    = Alnum
    | Alpha
    | Blank
    | Cntrl
    | Digit
    | Graph
    | Lower
    | Print
    | Punct
    | Space
    | Upper
    | XDigit
    deriving (Show, Eq, Enum, Bounded)

-- | A piece of a single path segment.
data Tok
    = -- | @*@: zero or more characters, never crossing a separator
      Star
    | One !CharMatch
    deriving (Show, Eq)

-- | One path segment of a pattern.
data Seg
    = -- | @**@ standing alone as a whole segment
      GlobStar
    | -- | Fast path: a segment holding no metacharacters
      Exact !Text
    | Seg ![Tok]
    deriving (Show, Eq)

newtype IgnorePattern = IgnorePattern [Seg]
    deriving (Show, Eq)

{- | One meaningful line of a @.gitignore@.

An unanchored rule matches an entry's /basename/ at any depth, so by
construction its pattern holds exactly one segment.
-}
data IgnoreRule = IgnoreRule
    { pattern :: !IgnorePattern
    , negated :: !Bool
    , dirOnly :: !Bool
    , anchored :: !Bool
    }
    deriving (Show, Eq)

-- | The rules of one @.gitignore@, in file order, and the directory they govern.
data IgnoreScope = IgnoreScope
    { base :: !AbsPath
    , rules :: ![IgnoreRule]
    }
    deriving (Show, Eq)

{- | Every @.gitignore@ under a project, merged.

INVARIANT: @scopes@ runs shallowest-first, which is what makes a deeper
@.gitignore@ override a shallower one during the fold in 'isIgnored'.
-}
data GitIgnore = GitIgnore
    { root :: !AbsPath
    , scopes :: ![IgnoreScope]
    }
    deriving (Show, Eq)

emptyGitIgnore :: AbsPath -> GitIgnore
emptyGitIgnore r = GitIgnore {root = r, scopes = []}

--------------------------------------------------------------------------------
-- Always-ignored directories
--------------------------------------------------------------------------------

{- | Directories and files never traversed, for any reason.

These are pruned greedily and are not even scanned for @.gitignore@ files:
they are large enough that walking them costs more than every other part of a
run combined. Unlike a gitignored path, no rule can re-include one.
-}
alwaysIgnoredNames :: [Text]
alwaysIgnoredNames =
    [ "node_modules"
    , ".git"
    , "dist"
    , ".next"
    , "next-env.d.ts"
    , ".next-env.d.ts"
    , "build"
    , "out"
    , ".output"
    , "storybook-static"
    , "coverage"
    , ".direnv"
    , ".devenv"
    , ".turbo"
    , ".cache"
    , ".parcel-cache"
    , ".yarn"
    , ".svelte-kit"
    , ".nuxt"
    , ".astro"
    , ".vercel"
    , ".wrangler"
    ]

alwaysIgnoredPaths :: Set OsPath
alwaysIgnoredPaths = Set.fromList . fmap encodeOsPath $ alwaysIgnoredNames

alwaysIgnored :: Entry -> Bool
alwaysIgnored = (`Set.member` alwaysIgnoredPaths) . takeFileName . (.osPath) . (.path)

--------------------------------------------------------------------------------
-- Loading
--------------------------------------------------------------------------------

gitIgnoreName :: OsPath
gitIgnoreName = [osp|.gitignore|]

{- | Walks the project once, collecting and parsing every @.gitignore@.

Pruning is by 'alwaysIgnored' only: this is the pass that discovers the rules,
so it cannot yet prune by them.
-}
loadGitIgnore :: (RoFileSystem :> es) => AbsPath -> Eff es GitIgnore
loadGitIgnore projectRoot =
    walkDir alwaysIgnored ignoreFile projectRoot
        >>= fmap (GitIgnore projectRoot . sortOn depth) . traverse readScope
  where
    ignoreFile e = e.path <$ guard (not e.isDir && takeFileName e.path.osPath == gitIgnoreName)
    depth = length . splitDirectories . (.osPath) . (.base)

-- | A @.gitignore@ that cannot be decoded contributes no rules.
readScope :: (RoFileSystem :> es) => AbsPath -> Eff es IgnoreScope
readScope file = mkScope . fromRight "" . decodeUtf8' <$> fsReadFile file
  where
    mkScope = IgnoreScope (absPathUnsafe . takeDirectory $ file.osPath) . parseIgnoreFile

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseIgnoreFile :: Text -> [IgnoreRule]
parseIgnoreFile = mapMaybe parseIgnoreRule . T.lines

{- | Parses one line, or 'Nothing' for a blank line or a comment.

Order matters and follows git: strip the line ending, drop unescaped trailing
spaces, reject comments and blanks, peel off @!@, then the directory-only
trailing @/@, and only then decide anchoring from whether a @/@ survives.
-}
parseIgnoreRule :: Text -> Maybe IgnoreRule
parseIgnoreRule raw = do
    let line = stripTrailingSpaces . T.dropWhileEnd (== '\r') $ raw
    guard . not . T.null $ line
    guard . not . T.isPrefixOf "#" $ line
    let (negated, unsigned) = peel "!" line
    let (dirOnly, body) = peelEnd "/" unsigned
    let anchored = T.isInfixOf "/" body
    guard . not . T.null $ body
    pure
        IgnoreRule
            { pattern = compilePattern . fromMaybe body . T.stripPrefix "/" $ body
            , negated
            , dirOnly
            , anchored
            }
  where
    peel p t = maybe (False, t) (True,) . T.stripPrefix p $ t
    peelEnd p t = maybe (False, t) (True,) . T.stripSuffix p $ t

{- | Drops trailing spaces, keeping one escaped by an odd run of backslashes.

@"foo "@ loses its space, @"foo\\ "@ keeps it, and @"foo\\\\ "@ loses it again
because the backslash there escapes a backslash.
-}
stripTrailingSpaces :: Text -> Text
stripTrailingSpaces t = case T.unsnoc t of
    Just (rest, ' ') | not . escaped $ rest -> stripTrailingSpaces rest
    _ -> t
  where
    escaped = odd . T.length . T.takeWhileEnd (== '\\')

compilePattern :: Text -> IgnorePattern
compilePattern = IgnorePattern . fmap compileSeg . T.splitOn "/"

compileSeg :: Text -> Seg
compileSeg "**" = GlobStar
compileSeg t = maybe (Exact t) collapse . parseMaybe (many pTok <* eof) $ t
  where
    collapse toks = maybe (Seg toks) Exact . allLiteral $ toks
    allLiteral = fmap T.pack . traverse litChar
    litChar = \case
        One (Lit c) -> Just c
        _ -> Nothing

type Parser = Parsec Void Text

pTok :: Parser Tok
pTok = choice [Star <$ char '*', One <$> pCharMatch]

-- A '[' that does not open a well-formed class falls back to a literal '['.
pCharMatch :: Parser CharMatch
pCharMatch = choice [AnyChar <$ char '?', try pClass, Lit <$> pLitChar]
  where
    pLitChar = (char '\\' *> anySingle) <|> satisfy (`notElem` ("*?" :: String))

pClass :: Parser CharMatch
pClass = between (char '[') (char ']') $ do
    negated <- isJust <$> optional (char '!' <|> char '^')
    -- A ']' immediately after the opening bracket is a literal, not a close.
    leading <- optional (ClassChar <$> char ']')
    items <- many pClassItem
    pure . Class negated $ maybeToList leading <> items

pClassItem :: Parser ClassItem
pClassItem = choice [try pPosix, try pRange, ClassChar <$> pClassChar]
  where
    pPosix = between (chunk "[:") (chunk ":]") (ClassPosix <$> pPosixClass)
    pRange = ClassRange <$> pClassChar <* char '-' <*> pClassChar

pClassChar :: Parser Char
pClassChar = (char '\\' *> anySingle) <|> satisfy (/= ']')

pPosixClass :: Parser PosixClass
pPosixClass = choice . fmap named $ [minBound .. maxBound]
  where
    named c = c <$ chunk (posixName c)

posixName :: PosixClass -> Text
posixName = \case
    Alnum -> "alnum"
    Alpha -> "alpha"
    Blank -> "blank"
    Cntrl -> "cntrl"
    Digit -> "digit"
    Graph -> "graph"
    Lower -> "lower"
    Print -> "print"
    Punct -> "punct"
    Space -> "space"
    Upper -> "upper"
    XDigit -> "xdigit"

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Inverse of 'parseIgnoreRule', up to the normalisation that parsing applies.
renderIgnoreRule :: IgnoreRule -> Text
renderIgnoreRule rule = prefix <> body <> suffix
  where
    prefix = bool "" "!" rule.negated <> bool "" "/" rule.anchored
    suffix = bool "" "/" rule.dirOnly
    body = T.intercalate "/" . fmap renderSeg $ segsOfPattern rule.pattern

renderSeg :: Seg -> Text
renderSeg = \case
    GlobStar -> "**"
    Exact t -> escapeLiterals t
    Seg toks -> foldMap renderTok toks

renderTok :: Tok -> Text
renderTok = \case
    Star -> "*"
    One AnyChar -> "?"
    One (Lit c) -> escapeLiterals . T.singleton $ c
    One (Class negated items) ->
        "[" <> bool "" "!" negated <> foldMap renderClassItem items <> "]"

renderClassItem :: ClassItem -> Text
renderClassItem = \case
    ClassChar c -> escapeInClass c
    ClassRange lo hi -> escapeInClass lo <> "-" <> escapeInClass hi
    ClassPosix c -> "[:" <> posixName c <> ":]"

escapeLiterals :: Text -> Text
escapeLiterals = T.concatMap escapeOne
  where
    escapeOne c = bool (T.singleton c) ("\\" <> T.singleton c) (c `elem` reserved)
    reserved = "*?[]\\! #" :: String

{- | @!@ and @^@ are escaped too: a class opening with either would otherwise
re-read as a negation rather than as a literal first member.
-}
escapeInClass :: Char -> Text
escapeInClass c = bool (T.singleton c) ("\\" <> T.singleton c) (c `elem` ("]\\-!^" :: String))

--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------

{- | Whether git would ignore this entry.

Self-contained: an entry inside an ignored directory is ignored, so this walks
the entry's whole ancestor chain rather than testing the entry alone. Callers
that already prune ignored directories as they descend will find the chain
short-circuits at the first component.
-}
isIgnored :: GitIgnore -> Entry -> Bool
isIgnored gi = any component . ancestry gi.root
  where
    component e = alwaysIgnored e || fromMaybe False (verdict gi e)

{- | The chain from the outermost component under the root down to the entry.

Every ancestor is a directory by construction; only the entry itself carries
the caller's own 'isDir'.
-}
ancestry :: AbsPath -> Entry -> [Entry]
ancestry projectRoot entry = fmap dirEntry (ancestorDirs projectRoot parentSegs) <> [entry]
  where
    rootDepth = length . splitDirectories $ projectRoot.osPath
    parentSegs = dropLast . drop rootDepth . splitDirectories $ entry.path.osPath
    dropLast = fromMaybe [] . viaNonEmpty init
    dirEntry p = Entry {path = p, isDir = True}

    ancestorDirs _ [] = []
    ancestorDirs from (s : rest) = let d = withAbsBaseUnsafe from s in d : ancestorDirs d rest

{- | The last matching rule wins, and deeper scopes are folded after shallower
ones so that they override. 'Nothing' means no rule in any scope matched.
-}
verdict :: GitIgnore -> Entry -> Maybe Bool
verdict gi entry = foldl' scopeVerdict Nothing gi.scopes
  where
    entrySegs = splitDirectories . (.osPath) $ entry.path

    scopeVerdict acc scope = maybe acc (applyRules acc scope.rules) (under scope)
    applyRules acc rs relSegs = foldl' (ruleVerdict relSegs) acc rs

    ruleVerdict relSegs acc rule =
        bool acc (Just . not $ rule.negated) (matchRule rule relSegs entry.isDir)

    under scope =
        let baseSegs = splitDirectories scope.base.osPath
            rest = drop (length baseSegs) entrySegs
         in fmap decodeOsPath rest <$ guard (baseSegs `isPrefixOf` entrySegs && not (null rest))

{- | Matches one rule against a path already made relative to its scope.

An anchored rule matches the relative path in full; an unanchored one matches
only the basename, which is what lets @*.log@ hit at any depth.
-}
matchRule :: IgnoreRule -> [Text] -> Bool -> Bool
matchRule rule relSegs isDirectory
    | rule.dirOnly && not isDirectory = False
    | rule.anchored = matchSegs segs relSegs
    | otherwise = maybe False (matchSegs segs . one) . viaNonEmpty last $ relSegs
  where
    segs = segsOfPattern rule.pattern

{- | @**@ spans zero or more segments, except as the final segment of a
pattern, where @foo/**@ means everything /inside/ @foo@ and so needs at least
one.
-}
matchSegs :: [Seg] -> [Text] -> Bool
matchSegs [] ps = null ps
matchSegs [GlobStar] ps = not . null $ ps
matchSegs segs@(GlobStar : ss) ps =
    matchSegs ss ps || case ps of
        [] -> False
        _ : rest -> matchSegs segs rest
matchSegs (_ : _) [] = False
matchSegs (Exact t : ss) (p : ps) = t == p && matchSegs ss ps
matchSegs (Seg toks : ss) (p : ps) = matchToks toks p && matchSegs ss ps

matchToks :: [Tok] -> Text -> Bool
matchToks [] t = T.null t
matchToks toks@(Star : ts) t =
    matchToks ts t || maybe False (matchToks toks . snd) (T.uncons t)
matchToks (One m : ts) t = maybe False step . T.uncons $ t
  where
    step (c, rest) = matchChar m c && matchToks ts rest

matchChar :: CharMatch -> Char -> Bool
matchChar m c = case m of
    Lit l -> l == c
    AnyChar -> True
    Class negated items -> negated /= any (inClass c) items

inClass :: Char -> ClassItem -> Bool
inClass c = \case
    ClassChar x -> x == c
    ClassRange lo hi -> lo <= c && c <= hi
    ClassPosix p -> inPosix p c

inPosix :: PosixClass -> Char -> Bool
inPosix p = case p of
    Alnum -> isAlphaNum
    Alpha -> isAlpha
    Blank -> \c -> c == '\t' || isSeparator c
    Cntrl -> isControl
    Digit -> isDigit
    Graph -> \c -> isPrint c && not (isSpace c)
    Lower -> isLower
    Print -> isPrint
    Punct -> \c -> isPunctuation c || isSymbol c
    Space -> isSpace
    Upper -> isUpper
    XDigit -> isHexDigit

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

segsOfPattern :: IgnorePattern -> [Seg]
segsOfPattern (IgnorePattern segs) = segs
