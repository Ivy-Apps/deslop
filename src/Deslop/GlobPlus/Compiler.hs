{- | Turning the text an author wrote into a pattern the matcher can run.

Compilation is where every rule about what a Glob+ pattern /may say/ lives, so
that "Deslop.GlobPlus" is left with only the question of what a valid pattern
/means/. Four of those rules exist to keep a pattern's meaning independent of
the path it is matched against:

* @**@ occupies a whole segment. Glued to text it would make the number of
  segments a pattern consumes depend on the input.
* A target's variable may not have @**@ on both sides. Nothing in such a
  pattern says which directory the variable names.
* Two variables in a segment need a literal between them, and a @*@ is not one
  because it can match nothing.
* A variable is a name written in a casing, and the spelling must say which -
  see @docs/adr/0006@.
* @..@ may only go back past a segment whose text the pattern determines, and
  only in a clause - the one pattern with a directory to be relative to. See
  @docs/adr/0012@.

The pattern is split on @\/@ before anything else, which is sound because a
variable token may not contain one. Each piece is then parsed on its own, so a
parse error names the segment it came from.
-}
module Deslop.GlobPlus.Compiler (
    -- * Compiling
    compileTargetPattern,
    compileClausePattern,
    compileExcludePattern,

    -- * Errors
    GlobPlusError (..),
    renderGlobPlusError,

    -- * Prose
    interpolate,
) where

import Data.Char (isAsciiUpper)
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Casing (casingName, decode, render, spelledIn)
import Deslop.GlobPlus
import Text.Megaparsec (MonadParsec (notFollowedBy), ParseErrorBundle, Parsec, between, choice, eof, errorBundlePretty, many, noneOf, parse, some, try)
import Text.Megaparsec.Char (char, string)

--------------------------------------------------------------------------------
-- 1. Errors
--------------------------------------------------------------------------------

data GlobPlusError
    = -- | The pattern is not valid Glob+ syntax at all.
      MalformedPattern Text (ParseErrorBundle Text Void)
    | -- | @{{Provider-Name}}@ - not a recognised casing.
      UnrecognisedCasing Text
    | -- | @{{provider}}@ - reads as more than one casing.
      AmbiguousCasing Text (NonEmpty Casing)
    | -- | @{{HTTPClient}}@ - word boundaries cannot be determined.
      ConsecutiveCapitals Text
    | -- | @{{target-dir}}@ - reserved, and only @{{TARGET_DIR}}@ is accepted.
      ReservedTargetDir Text
    | -- | @{{TARGET_DIR}}@ in a target pattern, where it cannot be captured.
      TargetDirInTargetPattern Text
    | -- | Any variable in an exclude pattern, which binds nothing.
      VariableInExcludePattern Text
    | -- | @..@ in a target pattern, which is matched against whole module ids.
      ParentDirInTargetPattern
    | -- | @..@ in an exclude pattern, for the same reason.
      ParentDirInExcludePattern
    | -- | @**\/..@ or @*\/..@ - the segment it would cancel names no one
      -- directory. Carries that segment as written.
      ParentDirPastWildcard Text
    | -- | A clause variable the rule's target pattern never captures.
      UnboundVariable VarName (Set VarName)
    | -- | @\/**View@ - a globstar glued to text inside a segment.
      GlobStarNotWholeSegment Text
    | -- | @**\/{{a}}\/**@ - nothing in the pattern says which segment @a@ is.
      UnanchoredVariable VarName
    | -- | @{{a}}{{b}}@ or @{{a}}*{{b}}@ - no literal separates the two.
      NoBoundaryBetween Text Text
    deriving (Show, Eq)

renderGlobPlusError :: GlobPlusError -> Text
renderGlobPlusError (MalformedPattern input bundle) =
    "invalid Glob+ pattern "
        <> quoted input
        <> "\n"
        <> T.strip (toText (errorBundlePretty bundle))
renderGlobPlusError (UnrecognisedCasing raw) =
    braced raw
        <> " is not written in a recognised casing.\n"
        <> "  A variable must be spelled in exactly one of:\n"
        <> "    PascalCase     e.g. {{ProviderName}}\n"
        <> "    camelCase      e.g. {{providerName}}\n"
        <> "    kebab-case     e.g. {{provider-name}}\n"
        <> "    CONSTANT_CASE  e.g. {{PROVIDER_NAME}}"
renderGlobPlusError (AmbiguousCasing raw casings) =
    braced raw
        <> " is ambiguous: a single-word name reads as both "
        <> T.intercalate " and " (casingName <$> toList casings)
        <> ".\n"
        <> "  Give the variable a name of at least two words, for example:\n"
        <> T.intercalate "\n" (("    " <>) . braced <$> ambiguitySuggestions raw casings)
renderGlobPlusError (ConsecutiveCapitals raw) =
    braced raw
        <> " contains consecutive capitals, so its word boundaries are ambiguous.\n"
        <> "  Capitalise only the first letter of each word, e.g. {{HttpClient}},\n"
        <> "  or use kebab-case, e.g. {{http-client}}."
renderGlobPlusError (ReservedTargetDir raw) =
    braced raw
        <> " is reserved.\n"
        <> "  The directory of the matched target is written {{TARGET_DIR}},\n"
        <> "  and no other spelling of that name is accepted."
renderGlobPlusError (TargetDirInTargetPattern raw) =
    braced raw
        <> " cannot be used in a target pattern.\n"
        <> "  {{TARGET_DIR}} is derived from the path the target matches,\n"
        <> "  so it only has a value in a clause pattern."
renderGlobPlusError (VariableInExcludePattern raw) =
    braced raw
        <> " cannot be used in an exclude pattern.\n"
        <> "  An exclude pattern filters the target and binds no variables.\n"
        <> "  Use a wildcard instead, e.g. * or **."
renderGlobPlusError ParentDirInTargetPattern =
    quoted parentDir
        <> " cannot be used in a target pattern.\n"
        <> "  A target is matched against whole module ids, so there is nothing\n"
        <> "  for "
        <> quoted parentDir
        <> " to be relative to. Write the path you mean, e.g. \"@/shared/**\".\n"
        <> relativeToTargetDir
renderGlobPlusError ParentDirInExcludePattern =
    quoted parentDir
        <> " cannot be used in an exclude pattern.\n"
        <> "  An exclude pattern filters the target and is matched against whole\n"
        <> "  module ids, so there is nothing for "
        <> quoted parentDir
        <> " to be relative to. Write\n"
        <> "  the path you mean, e.g. \"@/shared/**\".\n"
        <> relativeToTargetDir
renderGlobPlusError (ParentDirPastWildcard segment) =
    quoted parentDir
        <> " cannot go back past "
        <> quoted segment
        <> ".\n"
        <> "  "
        <> whyNoDirectory segment
        <> "\n"
        <> "  Write the directory you mean, or start from "
        <> braced targetDirKeyword
        <> "."
  where
    -- The two ways a segment can fail to name a directory read differently to
    -- an author, and 'checkParentDirs' spells a globstar as exactly @**@.
    whyNoDirectory segment'
        | segment' == globStar =
            quoted globStar
                <> " stands for zero or many segments, so there is no one\n"
                <> "  directory to go back from."
        | otherwise =
            "A segment containing \"*\" does not say which directory it is,\n"
                <> "  so there is no one directory to go back from."
renderGlobPlusError (UnboundVariable name bound) =
    "unknown variable "
        <> braced name.text
        <> ".\n"
        <> "  Variables bound by this rule's target: "
        <> renderBound
        <> maybe "" (\s -> "\n  Did you mean " <> braced s.text <> "?") (didYouMean name bound)
  where
    -- Set.toList is sorted, which keeps the message deterministic.
    renderBound
        | Set.null bound = "(none)"
        | otherwise = T.intercalate ", " ((.text) <$> Set.toList bound)
renderGlobPlusError (GlobStarNotWholeSegment segment) =
    quoted segment
        <> " glues ** to text inside a single path segment.\n"
        <> "  ** stands for zero or many whole segments, so it cannot be part of one.\n"
        <> "  Match within a segment with *, e.g. *View, or give ** a segment of its\n"
        <> "  own, e.g. **/*View."
renderGlobPlusError (UnanchoredVariable name) =
    braced name.text
        <> " has ** on both sides, so nothing in the pattern says which\n"
        <> "  path segment it names. A deeper tree would bind a different directory\n"
        <> "  than a shallow one, and neither would be the one you meant.\n"
        <> "  Anchor it: drop one of the **, or replace it with * to fix the depth."
renderGlobPlusError (NoBoundaryBetween left right) =
    braced left
        <> braced right
        <> " has no literal between the two variables, so there\n"
        <> "  is no way to tell where the first one ends. A * between them is not a\n"
        <> "  boundary either, because it can match nothing.\n"
        <> "  Separate them with a literal, e.g. "
        <> braced left
        <> "/"
        <> braced right
        <> "."

{- | The one place @..@ does belong, said the same way in both messages that
have to point at it.
-}
relativeToTargetDir :: Text
relativeToTargetDir =
    "  "
        <> quoted parentDir
        <> " belongs in a clause, where it is relative to the directory\n"
        <> "  of the file the target matched:\n"
        <> "    allows: \""
        <> braced targetDirKeyword
        <> "/../shared/**\""

{- | Suggests a two-word name for each casing the raw token could have meant,
so the author can pick the one they intended.
-}
ambiguitySuggestions :: Text -> NonEmpty Casing -> [Text]
ambiguitySuggestions raw casings = (`render` twoWords) <$> toList casings
  where
    -- The token is ambiguous, so any of its readings will do to name it after.
    twoWords = decode (head casings) raw <> ["name"]

didYouMean :: VarName -> Set VarName -> Maybe VarName
didYouMean name = find within . sortOn distance . Set.toList
  where
    distance candidate = editDistance name.text candidate.text
    within candidate = distance candidate <= 3

--------------------------------------------------------------------------------
-- 2. Compiling
--------------------------------------------------------------------------------

{- | Compiles the @target:@ of a rule. The variables it captures become the
only variables its clauses may reference.
-}
compileTargetPattern :: Text -> Either GlobPlusError CompiledTargetPattern
compileTargetPattern input = do
    segments <-
        traverse (resolveVars resolveTargetVar)
            =<< noParentDirs ParentDirInTargetPattern
            =<< parseSegments input
    traverse_ checkBoundaries segments
    checkAnchoring segments
    pure
        CompiledTargetPattern
            { segments = segments
            , minLength = minSegments segments
            , boundVars = Set.fromList [name | Segment parts <- segments, VarPart (TargetVar name _) <- parts]
            , source = input
            }

{- | Compiles a @uses@ \/ @forbids@ \/ @allows@ \/ @exists@ pattern against the
variables its rule's target pattern binds. Referencing anything else is an
error, so every lookup at match time is guaranteed to succeed.

A clause variable is /substituted/ rather than captured, so it is a literal by
the time anything is matched - which is why the anchoring rule does not apply
here and a clause may say @**\/{{provider-name}}\/**@ quite safely.

This is also the only pattern that may carry @..@, since it is the only one
with a directory - @{{TARGET_DIR}}@ - to be relative to.
-}
compileClausePattern :: Polarity -> Set VarName -> Text -> Either GlobPlusError CompiledClausePattern
compileClausePattern polarity bound input = do
    steps <- traverse (traverse (resolveVars resolveClauseVar)) =<< parseSegments input
    traverse_ (checkBound bound) (partsOf steps)
    checkParentDirs steps
    pure
        CompiledClausePattern
            { steps = steps
            , polarity = polarity
            , source = input
            }

-- | Compiles an @exclude:@ pattern, which is a plain glob over module ids.
compileExcludePattern :: Text -> Either GlobPlusError CompiledExcludePattern
compileExcludePattern input = do
    segments <-
        traverse (resolveVars (Left . VariableInExcludePattern))
            =<< noParentDirs ParentDirInExcludePattern
            =<< parseSegments input
    pure
        CompiledExcludePattern
            { segments = segments
            , minLength = minSegments segments
            , source = input
            }

--------------------------------------------------------------------------------
-- 3. Parsing
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

-- | Interprets what one segment's @{{ }}@ tokens name.
resolveVars ::
    (Text -> Either GlobPlusError var) ->
    PatternSegment Text ->
    Either GlobPlusError (PatternSegment var)
resolveVars resolve = traverse (traverse (traverse resolve))

{- | Every part of every segment, whatever it is nested inside. Validation that
looks at one part at a time has no use for the structure above it.
-}
partsOf :: [Step (PatternSegment var)] -> [SegPart var]
partsOf steps = [part | Step (Segment parts) <- steps, part <- parts]

{- | Drops the step wrapper from a pattern that may not carry @..@, naming the
error to report if one does.
-}
noParentDirs :: GlobPlusError -> [Step a] -> Either GlobPlusError [a]
noParentDirs cause = traverse unwrap
  where
    unwrap ParentDir = Left cause
    unwrap (Step step) = Right step

{- | Splits on @\/@ and parses each piece on its own. A variable token cannot
contain a @\/@, so splitting first can never cut one in half.
-}
parseSegments :: Text -> Either GlobPlusError [Step (PatternSegment Text)]
parseSegments input = traverse (parseStep input) (T.splitOn "/" input)

{- | @..@ is a whole segment or it is nothing. Unlike @**@, a dotted name has an
obvious ordinary reading - @..foo@, @a..b@ and @*.spec@ are all just text - so
only the exact token is structural, and nothing else about a dot is an error.
-}
parseStep :: Text -> Text -> Either GlobPlusError (Step (PatternSegment Text))
parseStep input piece
    | piece == parentDir = Right ParentDir
    | otherwise = Step <$> parseSegment input piece

parseSegment :: Text -> Text -> Either GlobPlusError (PatternSegment Text)
parseSegment input piece
    | piece == globStar = Right GlobStar
    | globStar `T.isInfixOf` piece = Left (GlobStarNotWholeSegment piece)
    | otherwise = Segment . mergeLits <$> first (MalformedPattern input) (parse (many pPart <* eof) "" piece)

mergeLits :: [SegPart var] -> [SegPart var]
mergeLits (Lit a : Lit b : rest) = mergeLits (Lit (a <> b) : rest)
mergeLits (part : rest) = part : mergeLits rest
mergeLits [] = []

{- | Parses the shape of a segment only. What is inside @{{ }}@ is carried
through verbatim and interpreted by 'resolveTargetVar' \/ 'resolveClauseVar',
so that casing diagnostics are ours rather than megaparsec's.
-}
pPart :: Parser (SegPart Text)
pPart = choice [pStar, VarPart <$> pRawVar, pLiteral]

pStar :: Parser (SegPart Text)
pStar = AnyChars <$ char '*'

pLiteral :: Parser (SegPart Text)
pLiteral = Lit . T.pack <$> some pLitChar
  where
    pLitChar = try (char '{' <* notFollowedBy (char '{')) <|> noneOf ['*', '{']

-- | Anything that is not structural is a name character, so that a bad name
-- yields our own casing diagnostic rather than a raw megaparsec error.
pRawVar :: Parser Text
pRawVar = between (string "{{") (string "}}") (T.pack <$> some (noneOf ['{', '}', '*', '/']))

--------------------------------------------------------------------------------
-- 4. Validation
--------------------------------------------------------------------------------

targetDirName :: VarName
targetDirName = VarName "target-dir"

resolveTargetVar :: Text -> Either GlobPlusError TargetVar
resolveTargetVar raw
    | namesTargetDir raw = Left (TargetDirInTargetPattern raw)
    | otherwise = uncurry TargetVar <$> resolveVar raw

resolveClauseVar :: Text -> Either GlobPlusError ClauseVar
resolveClauseVar raw
    | raw == targetDirKeyword = Right CTargetDir
    | namesTargetDir raw = Left (ReservedTargetDir raw)
    | otherwise = uncurry ClauseVar <$> resolveVar raw

resolveVar :: Text -> Either GlobPlusError (VarName, Casing)
resolveVar raw = do
    casing <- detectCasing raw
    checkConsecutiveCapitals casing raw
    pure (canonicalName casing raw, casing)

{- | Whether a token is any spelling of the reserved name. A token that is not
written in a single recognised casing cannot be one, and is left to
'resolveVar' to report against the casing rules instead.
-}
namesTargetDir :: Text -> Bool
namesTargetDir raw = case detectCasing raw of
    Right casing -> canonicalName casing raw == targetDirName
    Left _ -> False

{- | A token is written in the one casing it is a valid spelling of. Spelling
alone is enough for a name of two or more words; a single word such as
@provider@ reads as both camelCase and kebab-case, and is rejected.
-}
detectCasing :: Text -> Either GlobPlusError Casing
detectCasing raw = case filter (`spelledIn` raw) [minBound .. maxBound] of
    [casing] -> Right casing
    [] -> Left (UnrecognisedCasing raw)
    (c : cs) -> Left (AmbiguousCasing raw (c :| cs))

{- | @HTTPClient@ has no determinable word boundaries, which would make it a
different variable from @http-client@. Constant case is all capitals by
definition, so the check applies only where capitals carry meaning.
-}
checkConsecutiveCapitals :: Casing -> Text -> Either GlobPlusError ()
checkConsecutiveCapitals casing raw = case casing of
    PascalCase -> reject
    CamelCase -> reject
    KebabCase -> Right ()
    ConstantCase -> Right ()
  where
    reject
        | any bothUpper (T.zip raw (T.drop 1 raw)) = Left (ConsecutiveCapitals raw)
        | otherwise = Right ()
    bothUpper (a, b) = isAsciiUpper a && isAsciiUpper b

{- | Two variables in one segment need a literal between them: without one
there is no way to tell where the first ends. A @*@ does not count, because it
can match the empty string and so leaves the boundary exactly as undetermined
as it was.
-}
checkBoundaries :: PatternSegment TargetVar -> Either GlobPlusError ()
checkBoundaries GlobStar = Right ()
checkBoundaries (Segment parts) = adjacent (filter (/= AnyChars) parts)
  where
    adjacent (VarPart left : VarPart right : _) = Left (NoBoundaryBetween (spellTargetVar left) (spellTargetVar right))
    adjacent (_ : rest) = adjacent rest
    adjacent [] = Right ()

{- | A target's variable must have its segment fixed by the pattern. With a
globstar on both sides the path decides instead, so the same rule would name a
different directory in a shallow tree than in a deep one.

This is what lets the matcher treat globstar widths as an implementation
detail: with every variable anchored, no choice of widths can change what
anything binds.
-}
checkAnchoring :: [PatternSegment TargetVar] -> Either GlobPlusError ()
checkAnchoring segments = maybe (Right ()) (Left . UnanchoredVariable) unanchored
  where
    unanchored =
        viaNonEmpty head
            [ name
            | (index, Segment parts) <- indexed
            , globStarsWhere (< index)
            , globStarsWhere (> index)
            , VarPart (TargetVar name _) <- parts
            ]

    indexed = zip [0 :: Int ..] segments
    globStarsWhere side = any (\(index, segment) -> side index && segment == GlobStar) indexed

{- | Simulates the cancellation to find what each @..@ would go back past.

Each step contributes one token and each @..@ takes one back, which is never
more of the pattern than hydration itself has: a @{{TARGET_DIR}}@ step becomes
several segments there and one token here, so this reaches an earlier step
sooner than hydration can, and never later. That is what makes the check
sufficient - a @..@ hydration would let past a @**@ is one this rejects first,
so the resolution at match time is a plain drop and cannot fail.

Taking back nothing at all is the clamp, and is allowed. Taking back a segment
that names no one directory is not.
-}
checkParentDirs :: [Step (PatternSegment ClauseVar)] -> Either GlobPlusError ()
checkParentDirs = void . foldlM cancel []
  where
    cancel behind ParentDir = case behind of
        [] -> Right []
        (segment : earlier) -> earlier <$ namesOneDirectory segment
    cancel behind (Step segment) = Right (segment : behind)

    namesOneDirectory GlobStar = Left (ParentDirPastWildcard globStar)
    namesOneDirectory (Segment parts)
        | AnyChars `elem` parts = Left (ParentDirPastWildcard (spellSegment parts))
        | otherwise = Right ()

-- | Writes a compiled segment back out the way its author wrote it.
spellSegment :: [SegPart ClauseVar] -> Text
spellSegment = foldMap spellPart
  where
    spellPart (Lit literal) = literal
    spellPart AnyChars = "*"
    spellPart (VarPart CTargetDir) = braced targetDirKeyword
    spellPart (VarPart (ClauseVar name casing)) = braced (spellVar name casing)

checkBound :: Set VarName -> SegPart ClauseVar -> Either GlobPlusError ()
checkBound bound (VarPart (ClauseVar name _))
    | not (Set.member name bound) = Left (UnboundVariable name bound)
checkBound _ _ = Right ()

canonicalName :: Casing -> Text -> VarName
canonicalName casing = VarName . render KebabCase . decode casing

spellTargetVar :: TargetVar -> Text
spellTargetVar (TargetVar name casing) = spellVar name casing

--------------------------------------------------------------------------------
-- 5. Prose
--------------------------------------------------------------------------------

{- | Substitutes the variables in prose - a rule's @description@ or @fix@ -
with the values its target captured, each written in the casing its occurrence
is spelled in.

Prose is not a pattern: @*@ and @\/@ are ordinary characters here, and a token
that names nothing in scope is left exactly as written. A message can therefore
never be mangled by a typo, an unbound name, or braces that were only ever
meant to be read as braces.
-}
interpolate :: MatchEnv -> Text -> Text
interpolate env = go
  where
    go text =
        let (before, rest) = T.breakOn openBrace text
         in if T.null rest
                then before
                else before <> token (T.drop (T.length openBrace) rest)

    {- The opening braces are already consumed, so either the token resolves,
    or they are written back out and scanning resumes just after them - which
    is what lets a token nested inside an unrecognised one still be found. -}
    token rest = case T.breakOn closeBrace rest of
        (raw, closing)
            | not (T.null closing)
            , Just value <- resolve raw ->
                value <> go (T.drop (T.length closeBrace) closing)
        _ -> openBrace <> go rest

    -- What may stand between braces is 'pRawVar''s business, so a candidate is
    -- put back through it rather than judged by a second set of rules here.
    resolve raw = do
        name <- rightToMaybe (parse (pRawVar <* eof) "" (braced raw))
        var <- rightToMaybe (resolveClauseVar name)
        valueOf env var

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

openBrace, closeBrace :: Text
openBrace = "{{"
closeBrace = "}}"

-- | The parent-directory entry, spelled the way a filesystem spells it.
parentDir :: Text
parentDir = ".."

globStar :: Text
globStar = "**"

braced :: Text -> Text
braced t = openBrace <> t <> closeBrace

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

-- | Levenshtein distance, used only to suggest a near-miss variable name.
editDistance :: Text -> Text -> Int
editDistance source target =
    fromMaybe 0 . viaNonEmpty last $
        foldl' nextRow [0 .. T.length source] (T.unpack target)
  where
    nextRow row@(leading : rest) c = scanl (nextCost c) (leading + 1) (zip3 (T.unpack source) row rest)
    nextRow [] _ = []

    nextCost c left (sourceChar, diagonal, up) =
        (up + 1) `min` (left + 1) `min` (diagonal + if sourceChar == c then 0 else 1)
