module Deslop.GlobPlus (
    -- * Core Types
    Pattern,
    TargetPattern,
    ClausePattern,
    MatchEnv (..),
    Casing (..),

    -- * Compiled Types (For Reader Env)
    CompiledTargetPattern (..),
    CompiledClausePattern (..),

    -- * Parsing
    parseTargetPattern,
    parseClausePattern,

    -- * Compiling (Ahead-of-Time)
    compileTargetPattern,
    compileClausePattern,

    -- * Matching Engines (Hot Path)
    matchTarget,
    matchClause,

    -- * Expansion
    moduleFromGlob,
    renderClausePattern,
) where

import Data.Char (isUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Text.Megaparsec (MonadParsec (notFollowedBy), ParseErrorBundle, Parsec, between, choice, eof, many, noneOf, parse, some, try)
import Text.Megaparsec.Char (char, string)
import Text.Regex.TDFA (Regex, makeRegex, match)
import Text.Regex.TDFA.Text ()
import Text.Show (Show (..), showString, shows)

--------------------------------------------------------------------------------
-- 1. Core Types & Type Safety
--------------------------------------------------------------------------------

data Casing
    = -- | {{FileName}}
      PascalCase
    | -- | {{fileName}}
      CamelCase
    | -- | {{FILE_NAME}}
      ConstantCase
    | -- | {{file-name}}
      KebabCase
    deriving (Show, Eq, Ord)

-- | Variables allowed in a Target pattern (Strictly NO TARGET_DIR)
newtype TargetVar = TVar Casing
    deriving (Show, Eq, Ord)

-- | Variables allowed in a Rule/Dependent pattern
data ClauseVar
    = CVar Casing
    | -- | {{TARGET_DIR}}
      CTargetDir
    deriving (Show, Eq, Ord)

data Token var
    = Literal Text
    | Star
    | GlobStar
    | Var var
    deriving (Show, Eq, Functor)

newtype Pattern var = Pattern [Token var]
    deriving (Show, Eq)

type TargetPattern = Pattern TargetVar
type ClausePattern = Pattern ClauseVar

data MatchEnv = MatchEnv
    { targetDir :: Text
    , casings :: Map Casing Text
    }
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 2. Compiled Types (Performance Optimizations)
--------------------------------------------------------------------------------

data CompiledTargetPattern = CompiledTargetPattern
    { regex :: Regex
    , vars :: [Casing]
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

--------------------------------------------------------------------------------
-- 3. Megaparsec Parsers
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseTargetPattern :: Text -> Either (ParseErrorBundle Text Void) TargetPattern
parseTargetPattern = parse (Pattern <$> many (pToken pTargetVar) <* eof) ""

parseClausePattern :: Text -> Either (ParseErrorBundle Text Void) ClausePattern
parseClausePattern = parse (Pattern <$> many (pToken pClauseVar) <* eof) ""

pToken :: Parser var -> Parser (Token var)
pToken pVarParser = choice [try pGlobStar, pStar, Var <$> pVarParser, pLiteral]

pGlobStar, pStar :: Parser (Token var)
pGlobStar = GlobStar <$ string "**"
pStar = Star <$ char '*'

pLiteral :: Parser (Token var)
pLiteral = Literal . T.pack <$> some pLitChar
  where
    pLitChar = try (char '{' <* notFollowedBy (char '{')) <|> noneOf ['*', '{']

pTargetVar :: Parser TargetVar
pTargetVar = TVar <$> between (string "{{") (string "}}") pCasing

pClauseVar :: Parser ClauseVar
pClauseVar =
    between (string "{{") (string "}}") $
        choice
            [ CTargetDir <$ string "TARGET_DIR"
            , CVar <$> pCasing
            ]

pCasing :: Parser Casing
pCasing =
    choice
        [ PascalCase <$ string "FileName"
        , CamelCase <$ string "fileName"
        , ConstantCase <$ string "FILE_NAME"
        , KebabCase <$ string "file-name"
        ]

--------------------------------------------------------------------------------
-- 4. Ahead-of-Time Compilers
--------------------------------------------------------------------------------

compileTargetPattern :: TargetPattern -> CompiledTargetPattern
compileTargetPattern (Pattern tokens) =
    let regexStr = "^" <> T.concat (mapTokensGlob "(.*/)?" toRegex tokens) <> "$"
        extractedVars = [c | Var (TVar c) <- tokens]
        regexObj = makeRegex regexStr :: Regex
     in CompiledTargetPattern
            { regex = regexObj
            , vars = extractedVars
            , globCaptures = countGlobSlash tokens
            }
  where
    toRegex (Literal t) = escapeRegex t
    toRegex Star = "[^/]*"
    toRegex GlobStar = ".*"
    toRegex (Var (TVar PascalCase)) = "([A-Z][a-zA-Z0-9]*)"
    toRegex (Var (TVar CamelCase)) = "([a-z][a-zA-Z0-9]*)"
    toRegex (Var (TVar KebabCase)) = "([a-z0-9-]+)"
    toRegex (Var (TVar ConstantCase)) = "([A-Z0-9_]+)"

compileClausePattern :: ClausePattern -> CompiledClausePattern
compileClausePattern (Pattern tokens) =
    CompiledClausePattern
        { chunks =
            optimizeChunks $
                StaticChunk "^" : mapTokensGlob (StaticChunk "(.*/)?") toChunk tokens ++ [StaticChunk "$"]
        , rawTokens = tokens
        }
  where
    toChunk (Literal t) = StaticChunk (escapeRegex t)
    toChunk Star = StaticChunk "[^/]*"
    toChunk GlobStar = StaticChunk ".*"
    toChunk (Var v) = VarChunk v

    optimizeChunks [] = []
    optimizeChunks (StaticChunk a : StaticChunk b : rest) = optimizeChunks (StaticChunk (a <> b) : rest)
    optimizeChunks (x : xs) = x : optimizeChunks xs

--------------------------------------------------------------------------------
-- 5. Matchers (The Hot Path)
--------------------------------------------------------------------------------

matchTarget :: CompiledTargetPattern -> Text -> Maybe MatchEnv
matchTarget ctp targetPath =
    let (_, matched, _, captures) = match ctp.regex targetPath :: (Text, Text, Text, [Text])
        varCaptures = drop ctp.globCaptures captures
     in if matched /= "" && length varCaptures == length ctp.vars
            then
                let baseBindings = Map.fromList $ zip ctp.vars varCaptures
                    dir = getDirName targetPath
                 in Just $ MatchEnv {targetDir = dir, casings = enrichCasings baseBindings}
            else Nothing

getDirName :: Text -> Text
getDirName = maybe "." (T.intercalate "/" . init) . nonEmpty . T.splitOn "/"

-- TODO(perf): `matchClause` currently recompiles the hydrated regex on every call.
-- Fix: eta-reduce to `matchClause crp env = let regexObj = makeRegex ... in match regexObj`
-- so the Regex is compiled once when partially applied to `(crp, env)`, and bind
-- `matchClause p e` to a `let`/`where` name at each call site to guarantee sharing
-- across the inner loop (e.g. the transitive reachability traverse in RuleEnforcer).
matchClause :: CompiledClausePattern -> MatchEnv -> Text -> Bool
matchClause crp env targetPath =
    let regexStr = T.concat (map resolveChunk crp.chunks)
        -- Compiles the dynamically hydrated Text directly into a Regex
        regexObj = makeRegex regexStr :: Regex
     in match regexObj targetPath :: Bool
  where
    resolveChunk (StaticChunk s) = s
    resolveChunk (VarChunk CTargetDir) = escapeRegex env.targetDir
    resolveChunk (VarChunk (CVar c)) =
        maybe ".*" escapeRegex (Map.lookup c env.casings)

{- | Expands a rule pattern into a concrete module path by substituting
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
    expand (Var (CVar casing)) = Just $ fromMaybe "" (Map.lookup casing env.casings)

{- | Renders a rule pattern as a human-readable string by substituting
variables from the MatchEnv and keeping wildcards (* or **) literally.
-}
renderClausePattern :: MatchEnv -> CompiledClausePattern -> Text
renderClausePattern env crp = T.concat (map renderToken crp.rawTokens)
  where
    renderToken (Literal t) = t
    renderToken Star = "*"
    renderToken GlobStar = "**"
    renderToken (Var CTargetDir) = env.targetDir
    renderToken (Var (CVar casing)) = fromMaybe ("{{" <> caseName casing <> "}}") (Map.lookup casing env.casings)
    caseName PascalCase = "FileName"
    caseName CamelCase = "fileName"
    caseName KebabCase = "file-name"
    caseName ConstantCase = "FILE_NAME"

--------------------------------------------------------------------------------
-- 6. Case Tokenization & Enrichment
--------------------------------------------------------------------------------

enrichCasings :: Map Casing Text -> Map Casing Text
enrichCasings baseMap =
    case listToMaybe (Map.elems baseMap) of
        Nothing -> baseMap
        Just val -> Map.union baseMap (Map.fromList $ generateAllCasings val)

generateAllCasings :: Text -> [(Casing, Text)]
generateAllCasings txt =
    let tokens = tokenizeCase txt
     in [ (PascalCase, toPascalCase tokens)
        , (CamelCase, toCamelCase tokens)
        , (KebabCase, toKebabCase tokens)
        , (ConstantCase, toConstantCase tokens)
        ]

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

{- | Counts how many times the /**/ idiom appears in a token list —
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
