module Deslop.GlobPlus (
    -- * Core Types
    MatchEnv (..),
    Casing (..),
    VarName (..),
    CasedName (..),
    casedAs,

    -- * Compiled Types (For Reader Env)
    CompiledTargetPattern (..),
    CompiledClausePattern (..),
    CompiledExcludePattern (..),

    -- * Compilation Errors
    GlobPlusError (..),
    renderGlobPlusError,

    -- * Compiling (Ahead-of-Time)
    compileTargetPattern,
    compileClausePattern,
    compileExcludePattern,
    boundVars,

    -- * Matching Engines (Hot Path)
    matchTarget,
    matchClause,
    matchExclude,

    -- * Expansion
    moduleFromGlob,
    renderClausePattern,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isUpper)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Megaparsec (MonadParsec (notFollowedBy), ParseErrorBundle, Parsec, between, choice, eof, errorBundlePretty, many, noneOf, parse, some, try)
import Text.Megaparsec.Char (char, string)
import Text.Regex.TDFA (Regex, makeRegex, match)
import Text.Regex.TDFA.Text ()
import Text.Show (Show (..), showString, shows)

--------------------------------------------------------------------------------
-- 1. Core Types & Type Safety
--------------------------------------------------------------------------------

{- | How a variable is spelled. A variable is written in exactly one casing at
each occurrence, and the spelling alone determines which one - there is no
separate annotation.
-}
data Casing
    = -- | @ProviderName@
      PascalCase
    | -- | @providerName@
      CamelCase
    | -- | @PROVIDER_NAME@
      ConstantCase
    | -- | @provider-name@
      KebabCase
    deriving (Show, Eq, Ord, Enum, Bounded)

{- | The identity of a variable, canonicalised to kebab-case words. All four
spellings of a name denote the same variable: @{{ProviderName}}@,
@{{providerName}}@, @{{provider-name}}@ and @{{PROVIDER_NAME}}@ are all
'VarName' @\"provider-name\"@.
-}
newtype VarName = VarName {text :: Text}
    deriving (Show, Eq, Ord)

-- | A bound variable: one value, available in every casing.
data CasedName = CasedName
    { pascal :: Text
    , camel :: Text
    , kebab :: Text
    , constant :: Text
    }
    deriving (Show, Eq)

casedAs :: Casing -> CasedName -> Text
casedAs PascalCase n = n.pascal
casedAs CamelCase n = n.camel
casedAs KebabCase n = n.kebab
casedAs ConstantCase n = n.constant

-- | A variable occurrence in a target pattern. Strictly no @{{TARGET_DIR}}@.
data TargetVar = TargetVar VarName Casing
    deriving (Show, Eq, Ord)

-- | A variable occurrence in a clause pattern.
data ClauseVar
    = ClauseVar VarName Casing
    | -- | @{{TARGET_DIR}}@
      CTargetDir
    deriving (Show, Eq, Ord)

data Token var
    = Literal Text
    | Star
    | GlobStar
    | Var var
    deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Pattern var = Pattern [Token var]
    deriving (Show, Eq)

type TargetPattern = Pattern TargetVar
type ClausePattern = Pattern ClauseVar

{- | An exclude pattern is a plain glob. 'Void' makes the 'Var' constructor
uninhabited, so a variable in an exclude pattern is unrepresentable.
-}
type ExcludePattern = Pattern Void

data MatchEnv = MatchEnv
    { targetDir :: Text
    , variables :: Map VarName CasedName
    }
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. Compiled Types (Performance Optimizations)
--------------------------------------------------------------------------------

data CompiledTargetPattern = CompiledTargetPattern
    { regex :: Regex
    , vars :: [(VarName, Casing)] -- one entry per occurrence, in capture order
    , globCaptures :: Int
    }

instance Show CompiledTargetPattern where
    showsPrec _ ctp =
        showString "CompiledTargetPattern {regex = <regex>, vars = "
            . shows ctp.vars
            . showString ", globCaptures = "
            . shows ctp.globCaptures
            . showString "}"

data ClauseChunk
    = StaticChunk Text
    | VarChunk ClauseVar
    deriving (Show, Eq)

data CompiledClausePattern = CompiledClausePattern
    { chunks :: [ClauseChunk] -- regex form for matchClause (hot path)
    , rawTokens :: [Token ClauseVar] -- original tokens for moduleFromGlob
    }
    deriving (Show, Eq)

newtype CompiledExcludePattern = CompiledExcludePattern
    { regex :: Regex
    }

instance Show CompiledExcludePattern where
    showsPrec _ _ = showString "CompiledExcludePattern {regex = <regex>}"

--------------------------------------------------------------------------------
-- 3. Compilation Errors
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
    | -- | @{{a}}{{b}}@ in a target pattern - no boundary between captures.
      AdjacentVariables Text Text
    | -- | A clause variable the rule's target pattern never captures.
      UnboundVariable VarName (Set VarName)
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
renderGlobPlusError (AdjacentVariables left right) =
    braced left
        <> braced right
        <> " has no boundary between the two variables,\n"
        <> "  so there is no way to tell where the first one ends.\n"
        <> "  Separate them with a literal, e.g. "
        <> braced left
        <> "/"
        <> braced right
        <> "."
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

casingName :: Casing -> Text
casingName PascalCase = "PascalCase"
casingName CamelCase = "camelCase"
casingName KebabCase = "kebab-case"
casingName ConstantCase = "CONSTANT_CASE"

{- | Suggests a two-word name for each casing the raw token could have meant,
so the author can pick the one they intended.
-}
ambiguitySuggestions :: Text -> NonEmpty Casing -> [Text]
ambiguitySuggestions raw casings = spellWords twoWords <$> toList casings
  where
    twoWords = tokenizeCase raw <> ["name"]

didYouMean :: VarName -> Set VarName -> Maybe VarName
didYouMean name = find within . sortOn distance . Set.toList
  where
    distance candidate = editDistance name.text candidate.text
    within candidate = distance candidate <= 3

braced :: Text -> Text
braced t = "{{" <> t <> "}}"

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

--------------------------------------------------------------------------------
-- 4. Ahead-of-Time Compilers
--------------------------------------------------------------------------------

{- | Compiles the @target:@ of a rule. The variables it captures become the
only variables its clauses may reference.
-}
compileTargetPattern :: Text -> Either GlobPlusError CompiledTargetPattern
compileTargetPattern input = do
    Pattern tokens <- parseTargetPattern input
    checkAdjacency tokens
    pure
        CompiledTargetPattern
            { regex = makeRegex (globRegex targetVarRegex tokens) :: Regex
            , vars = [(name, casing) | Var (TargetVar name casing) <- tokens]
            , globCaptures = countGlobSlash tokens
            }
  where
    targetVarRegex (TargetVar _ casing) = captureRegex casing

{- | Compiles a @uses@ \/ @forbids@ \/ @allows@ \/ @exists@ pattern against the
variables its rule's target pattern binds. Referencing anything else is an
error, so every lookup at match time is guaranteed to succeed.
-}
compileClausePattern :: Set VarName -> Text -> Either GlobPlusError CompiledClausePattern
compileClausePattern bound input = do
    Pattern tokens <- parseClausePattern input
    traverse_ (checkBound bound) tokens
    pure
        CompiledClausePattern
            { chunks =
                mergeStaticChunks $
                    StaticChunk "^" : mapTokensGlob (StaticChunk "(.*/)?") toChunk tokens <> [StaticChunk "$"]
            , rawTokens = tokens
            }
  where
    toChunk (Literal t) = StaticChunk (escapeRegex t)
    toChunk Star = StaticChunk "[^/]*"
    toChunk GlobStar = StaticChunk ".*"
    toChunk (Var v) = VarChunk v

    mergeStaticChunks (StaticChunk a : StaticChunk b : rest) = mergeStaticChunks (StaticChunk (a <> b) : rest)
    mergeStaticChunks (x : xs) = x : mergeStaticChunks xs
    mergeStaticChunks [] = []

-- | Compiles an @exclude:@ pattern, which is a plain glob over module ids.
compileExcludePattern :: Text -> Either GlobPlusError CompiledExcludePattern
compileExcludePattern input = do
    Pattern tokens <- parseExcludePattern input
    pure CompiledExcludePattern {regex = makeRegex (globRegex absurd tokens) :: Regex}

boundVars :: CompiledTargetPattern -> Set VarName
boundVars ctp = Set.fromList (fst <$> ctp.vars)

globRegex :: (var -> Text) -> [Token var] -> Text
globRegex varRegex tokens = "^" <> T.concat (mapTokensGlob "(.*/)?" toRegex tokens) <> "$"
  where
    toRegex (Literal t) = escapeRegex t
    toRegex Star = "[^/]*"
    toRegex GlobStar = ".*"
    toRegex (Var v) = varRegex v

-- | The capture group a variable contributes to a target pattern's regex.
captureRegex :: Casing -> Text
captureRegex PascalCase = "([A-Z][a-zA-Z0-9]*)"
captureRegex CamelCase = "([a-z][a-zA-Z0-9]*)"
captureRegex KebabCase = "([a-z0-9-]+)"
captureRegex ConstantCase = "([A-Z0-9_]+)"

--------------------------------------------------------------------------------
-- 5. Megaparsec Parsers
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseTargetPattern :: Text -> Either GlobPlusError TargetPattern
parseTargetPattern = resolveVars resolveTargetVar <=< parsePattern

parseClausePattern :: Text -> Either GlobPlusError ClausePattern
parseClausePattern = resolveVars resolveClauseVar <=< parsePattern

parseExcludePattern :: Text -> Either GlobPlusError ExcludePattern
parseExcludePattern = resolveVars (Left . VariableInExcludePattern) <=< parsePattern

resolveVars :: (Text -> Either GlobPlusError var) -> Pattern Text -> Either GlobPlusError (Pattern var)
resolveVars resolve (Pattern tokens) = Pattern <$> traverse (traverse resolve) tokens

{- | Parses the shape of a pattern only. What is inside @{{ }}@ is carried
through verbatim and interpreted by 'resolveTargetVar' \/ 'resolveClauseVar',
so that casing diagnostics are ours rather than megaparsec's.
-}
parsePattern :: Text -> Either GlobPlusError (Pattern Text)
parsePattern input =
    first (MalformedPattern input) $
        parse (Pattern <$> many pToken <* eof) "" input

pToken :: Parser (Token Text)
pToken = choice [try pGlobStar, pStar, Var <$> pRawVar, pLiteral]

pGlobStar, pStar :: Parser (Token Text)
pGlobStar = GlobStar <$ string "**"
pStar = Star <$ char '*'

pLiteral :: Parser (Token Text)
pLiteral = Literal . T.pack <$> some pLitChar
  where
    pLitChar = try (char '{' <* notFollowedBy (char '{')) <|> noneOf ['*', '{']

-- | Anything that is not structural is a name character, so that a bad name
-- yields our own casing diagnostic rather than a raw megaparsec error.
pRawVar :: Parser Text
pRawVar = between (string "{{") (string "}}") (T.pack <$> some (noneOf ['{', '}', '*', '/']))

--------------------------------------------------------------------------------
-- 6. Validation
--------------------------------------------------------------------------------

targetDirKeyword :: Text
targetDirKeyword = "TARGET_DIR"

targetDirName :: VarName
targetDirName = VarName "target-dir"

resolveTargetVar :: Text -> Either GlobPlusError TargetVar
resolveTargetVar raw
    | canonicalName raw == targetDirName = Left (TargetDirInTargetPattern raw)
    | otherwise = uncurry TargetVar <$> resolveVar raw

resolveClauseVar :: Text -> Either GlobPlusError ClauseVar
resolveClauseVar raw
    | raw == targetDirKeyword = Right CTargetDir
    | canonicalName raw == targetDirName = Left (ReservedTargetDir raw)
    | otherwise = uncurry ClauseVar <$> resolveVar raw

resolveVar :: Text -> Either GlobPlusError (VarName, Casing)
resolveVar raw = do
    casing <- detectCasing raw
    checkConsecutiveCapitals casing raw
    pure (canonicalName raw, casing)

{- | A token is written in the one casing it is a valid spelling of. Spelling
alone is enough for a name of two or more words; a single word such as
@provider@ reads as both camelCase and kebab-case, and is rejected.
-}
detectCasing :: Text -> Either GlobPlusError Casing
detectCasing raw = case filter (`spells` raw) [minBound .. maxBound] of
    [casing] -> Right casing
    [] -> Left (UnrecognisedCasing raw)
    (c : cs) -> Left (AmbiguousCasing raw (c :| cs))

spells :: Casing -> Text -> Bool
spells PascalCase = startsWith isAsciiUpper isAsciiAlphaNum
spells CamelCase = startsWith isAsciiLower isAsciiAlphaNum
spells KebabCase = separatedBy '-' (\c -> isAsciiLower c || isDigit c)
spells ConstantCase = separatedBy '_' (\c -> isAsciiUpper c || isDigit c)

startsWith :: (Char -> Bool) -> (Char -> Bool) -> Text -> Bool
startsWith isFirst isRest t = case T.uncons t of
    Just (c, rest) -> isFirst c && T.all isRest rest
    Nothing -> False

separatedBy :: Char -> (Char -> Bool) -> Text -> Bool
separatedBy sep isBody t =
    all (\segment -> not (T.null segment) && T.all isBody segment) $
        T.splitOn (T.singleton sep) t

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

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

{- | Two variables with nothing between them give the regex no boundary to
split on. A literal separator that both can consume is allowed: the leftmost
variable binds greedily, which is documented behaviour.
-}
checkAdjacency :: [Token TargetVar] -> Either GlobPlusError ()
checkAdjacency (Var left : Var right : _) = Left (AdjacentVariables (spellTargetVar left) (spellTargetVar right))
checkAdjacency (_ : rest) = checkAdjacency rest
checkAdjacency [] = Right ()

checkBound :: Set VarName -> Token ClauseVar -> Either GlobPlusError ()
checkBound bound (Var (ClauseVar name _))
    | not (Set.member name bound) = Left (UnboundVariable name bound)
checkBound _ _ = Right ()

canonicalName :: Text -> VarName
canonicalName = VarName . toKebabCase . tokenizeCase

spellTargetVar :: TargetVar -> Text
spellTargetVar (TargetVar name casing) = spellVar name casing

-- | Writes a variable's canonical name back out in the given casing.
spellVar :: VarName -> Casing -> Text
spellVar name = spellWords (tokenizeCase name.text)

spellWords :: [Text] -> Casing -> Text
spellWords tokens PascalCase = toPascalCase tokens
spellWords tokens CamelCase = toCamelCase tokens
spellWords tokens KebabCase = toKebabCase tokens
spellWords tokens ConstantCase = toConstantCase tokens

--------------------------------------------------------------------------------
-- 7. Matchers (The Hot Path)
--------------------------------------------------------------------------------

matchTarget :: CompiledTargetPattern -> Text -> Maybe MatchEnv
matchTarget ctp targetPath =
    let (_, matched, _, captures) = match ctp.regex targetPath :: (Text, Text, Text, [Text])
        varCaptures = drop ctp.globCaptures captures
     in if matched /= "" && length varCaptures == length ctp.vars
            then
                MatchEnv (getDirName targetPath)
                    <$> bindVariables (zip ctp.vars varCaptures)
            else Nothing

{- | Groups every capture by the variable it belongs to. A variable may occur
more than once - @{{provider-name}}\/{{ProviderName}}View@ - in which case all
its captures must denote the same name, or the target does not match.
-}
bindVariables :: [((VarName, Casing), Text)] -> Maybe (Map VarName CasedName)
bindVariables captures = traverse bindOne grouped
  where
    grouped = Map.fromListWith (<>) [(name, [(casing, value)]) | ((name, casing), value) <- captures]

    bindOne occurrences = do
        wordsOf <- agreedWords (snd <$> occurrences)
        pure (foldl' overlay (casedNameFromWords wordsOf) occurrences)

    agreedWords values = case tokenizeCase <$> values of
        [] -> Nothing
        (w : ws) -> guard (all (== w) ws) $> w

    overlay named (PascalCase, value) = named {pascal = value}
    overlay named (CamelCase, value) = named {camel = value}
    overlay named (KebabCase, value) = named {kebab = value}
    overlay named (ConstantCase, value) = named {constant = value}

matchExclude :: CompiledExcludePattern -> Text -> Bool
matchExclude cep targetPath = match cep.regex targetPath :: Bool

getDirName :: Text -> Text
getDirName = maybe "." (T.intercalate "/" . init) . nonEmpty . T.splitOn "/"

-- TODO(perf): `matchClause` currently recompiles the hydrated regex on every call.
-- Fix: eta-reduce to `matchClause crp env = let regexObj = makeRegex ... in match regexObj`
-- so the Regex is compiled once when partially applied to `(crp, env)`, and bind
-- `matchClause p e` to a `let`/`where` name at each call site to guarantee sharing
-- across the inner loop (e.g. the transitive reachability traverse in RuleEnforcer).
matchClause :: CompiledClausePattern -> MatchEnv -> Text -> Bool
matchClause crp env targetPath = case traverse resolveChunk crp.chunks of
    -- Compilation guarantees every variable is bound, so Nothing is unreachable.
    -- Failing closed keeps an impossible state from silently widening a rule.
    Nothing -> False
    Just parts -> match (makeRegex (T.concat parts) :: Regex) targetPath :: Bool
  where
    resolveChunk (StaticChunk s) = Just s
    resolveChunk (VarChunk CTargetDir) = Just (escapeRegex env.targetDir)
    resolveChunk (VarChunk (ClauseVar name casing)) =
        escapeRegex . casedAs casing <$> Map.lookup name env.variables

--------------------------------------------------------------------------------
-- 8. Expansion
--------------------------------------------------------------------------------

{- | Expands a clause pattern into a concrete module path by substituting
variables from the MatchEnv. Returns Nothing if the pattern contains
wildcards (* or **), which cannot be deterministically expanded.
-}
moduleFromGlob :: MatchEnv -> CompiledClausePattern -> Maybe Text
moduleFromGlob env crp = T.concat <$> traverse expand crp.rawTokens
  where
    expand (Literal t) = Just t
    expand Star = Nothing
    expand GlobStar = Nothing
    expand (Var CTargetDir) = Just env.targetDir
    expand (Var (ClauseVar name casing)) = casedAs casing <$> Map.lookup name env.variables

{- | Renders a clause pattern as a human-readable string by substituting
variables from the MatchEnv and keeping wildcards (* or **) literally.
-}
renderClausePattern :: MatchEnv -> CompiledClausePattern -> Text
renderClausePattern env crp = T.concat (renderToken <$> crp.rawTokens)
  where
    renderToken (Literal t) = t
    renderToken Star = "*"
    renderToken GlobStar = "**"
    renderToken (Var CTargetDir) = env.targetDir
    renderToken (Var (ClauseVar name casing)) =
        maybe (braced (spellVar name casing)) (casedAs casing) (Map.lookup name env.variables)

--------------------------------------------------------------------------------
-- 9. Case Tokenization
--------------------------------------------------------------------------------

casedNameFromWords :: [Text] -> CasedName
casedNameFromWords tokens =
    CasedName
        { pascal = toPascalCase tokens
        , camel = toCamelCase tokens
        , kebab = toKebabCase tokens
        , constant = toConstantCase tokens
        }

tokenizeCase :: Text -> [Text]
tokenizeCase txt =
    let segments = concatMap (filter (not . T.null) . T.splitOn "_") (T.splitOn "-" txt)
     in concatMap processSegment segments
  where
    -- An all-uppercase segment (e.g. "MAX", "HTTP") is one word; mixed-case
    -- segments (e.g. "UserProfile") are split on CamelCase boundaries.
    processSegment seg
        | T.all isUpper seg = [T.toLower seg]
        | otherwise =
            filter (not . T.null)
                . map T.toLower
                . T.words
                . T.concatMap
                    (\c -> if isUpper c then " " <> T.singleton c else T.singleton c)
                $ seg

toPascalCase, toCamelCase, toKebabCase, toConstantCase :: [Text] -> Text
toPascalCase = T.concat . map capitalize
toCamelCase [] = ""
toCamelCase (x : xs) = x <> T.concat (map capitalize xs)
toKebabCase = T.intercalate "-"
toConstantCase = T.intercalate "_" . map T.toUpper

capitalize :: Text -> Text
capitalize t = case T.uncons t of
    Nothing -> ""
    Just (c, cs) -> (T.toUpper . T.singleton $ c) <> cs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- | Like 'map' over a token list, but absorbs the /**/ glob idiom:
when 'GlobStar' is immediately followed by a 'Literal' whose text starts
with '/', the leading '/' is stripped and 'slashAbsorbed' is emitted in
place of applying 'f' to 'GlobStar'.  This lets ** match zero path segments
so that e.g. @a\/**\/*@ matches @a\/x@ in addition to @a\/x\/y@.
-}
mapTokensGlob :: a -> (Token v -> a) -> [Token v] -> [a]
mapTokensGlob slashAbsorbed f = go
  where
    go [] = []
    go (GlobStar : Literal l : rest)
        | Just l' <- T.stripPrefix "/" l =
            slashAbsorbed : go (Literal l' : rest)
    go (t : rest) = f t : go rest

{- | Counts how many times the /**/ idiom appears in a token list -
i.e. GlobStar immediately followed by a Literal starting with '/'.
Must mirror the recursion in 'mapTokensGlob' so the count matches the
number of extra capture groups introduced by the (.*)? replacement.
-}
countGlobSlash :: [Token v] -> Int
countGlobSlash [] = 0
countGlobSlash (GlobStar : Literal l : rest)
    | Just l' <- T.stripPrefix "/" l = 1 + countGlobSlash (Literal l' : rest)
countGlobSlash (_ : rest) = countGlobSlash rest

escapeRegex :: Text -> Text
escapeRegex =
    T.concatMap
        ( \c ->
            if c `elem` ("\\^$.|?*+()[]{}" :: String)
                then "\\" <> T.singleton c
                else T.singleton c
        )

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
