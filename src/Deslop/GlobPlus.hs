module Deslop.GlobPlus (
    -- * Core Types
    Pattern,
    TargetPattern,
    RulePattern,
    MatchEnv (..),
    Casing (..),

    -- * Compiled Types (For Reader Env)
    CompiledTargetPattern (..),
    CompiledRulePattern (..),

    -- * Parsing
    parseTargetPattern,
    parseRulePattern,

    -- * Compiling (Ahead-of-Time)
    compileTargetPattern,
    compileRulePattern,

    -- * Matching Engines (Hot Path)
    matchTarget,
    matchRule,
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
      CamelCase
    | -- | {{fileName}}
      LowerCamelCase
    | -- | {{FILE_NAME}}
      ConstantCase
    | -- | {{file-name}}
      KebabCase
    deriving (Show, Eq, Ord)

-- | Variables allowed in a Target pattern (Strictly NO TARGET_DIR)
newtype TargetVar = TVar Casing
    deriving (Show, Eq, Ord)

-- | Variables allowed in a Rule/Dependent pattern
data RuleVar
    = RVar Casing
    | -- | {{TARGET_DIR}}
      RTargetDir
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
type RulePattern = Pattern RuleVar

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
    }

instance Show CompiledTargetPattern where
    showsPrec _ ctp =
        showString "CompiledTargetPattern {regex = <regex>, vars = "
            . shows ctp.vars
            . showString "}"

data RuleChunk
    = StaticChunk Text
    | VarChunk RuleVar
    deriving (Show, Eq)

newtype CompiledRulePattern = CompiledRulePattern
    {chunks :: [RuleChunk]}
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 3. Megaparsec Parsers
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseTargetPattern :: Text -> Either (ParseErrorBundle Text Void) TargetPattern
parseTargetPattern = parse (Pattern <$> many (pToken pTargetVar) <* eof) ""

parseRulePattern :: Text -> Either (ParseErrorBundle Text Void) RulePattern
parseRulePattern = parse (Pattern <$> many (pToken pRuleVar) <* eof) ""

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

pRuleVar :: Parser RuleVar
pRuleVar =
    between (string "{{") (string "}}") $
        choice
            [ RTargetDir <$ string "TARGET_DIR"
            , RVar <$> pCasing
            ]

pCasing :: Parser Casing
pCasing =
    choice
        [ CamelCase <$ string "FileName"
        , LowerCamelCase <$ string "fileName"
        , ConstantCase <$ string "FILE_NAME"
        , KebabCase <$ string "file-name"
        ]

--------------------------------------------------------------------------------
-- 4. Ahead-of-Time Compilers
--------------------------------------------------------------------------------

compileTargetPattern :: TargetPattern -> CompiledTargetPattern
compileTargetPattern (Pattern tokens) =
    let regexStr = "^" <> T.concat (map toRegex tokens) <> "$"
        extractedVars = [c | Var (TVar c) <- tokens]
        -- Typeclass infers `Text` as the source!
        regexObj = makeRegex regexStr :: Regex
     in CompiledTargetPattern {regex = regexObj, vars = extractedVars}
  where
    toRegex (Literal t) = escapeRegex t
    toRegex Star = "[^/]*"
    toRegex GlobStar = ".*"
    toRegex (Var (TVar CamelCase)) = "([A-Z][a-zA-Z0-9]*)"
    toRegex (Var (TVar LowerCamelCase)) = "([a-z][a-zA-Z0-9]*)"
    toRegex (Var (TVar KebabCase)) = "([a-z0-9-]+)"
    toRegex (Var (TVar ConstantCase)) = "([A-Z0-9_]+)"

compileRulePattern :: RulePattern -> CompiledRulePattern
compileRulePattern (Pattern tokens) =
    let optimizedChunks = StaticChunk "^" : map toChunk tokens ++ [StaticChunk "$"]
     in CompiledRulePattern {chunks = optimizeChunks optimizedChunks}
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
     in if matched /= "" && length captures == length ctp.vars
            then
                let baseBindings = Map.fromList $ zip ctp.vars captures
                    dir = getDirName targetPath
                 in Just $ MatchEnv {targetDir = dir, casings = enrichCasings baseBindings}
            else Nothing

getDirName :: Text -> Text
getDirName = maybe "." (T.intercalate "/" . init) . nonEmpty . T.splitOn "/"

matchRule :: CompiledRulePattern -> MatchEnv -> Text -> Bool
matchRule crp env targetPath =
    let regexStr = T.concat (map resolveChunk crp.chunks)
        -- Compiles the dynamically hydrated Text directly into a Regex
        regexObj = makeRegex regexStr :: Regex
     in match regexObj targetPath :: Bool
  where
    resolveChunk (StaticChunk s) = s
    resolveChunk (VarChunk RTargetDir) = escapeRegex env.targetDir
    resolveChunk (VarChunk (RVar c)) =
        case Map.lookup c env.casings of
            Just val -> escapeRegex val
            Nothing -> ".*"

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
     in [ (CamelCase, toCamelCase tokens)
        , (LowerCamelCase, toLowerCamelCase tokens)
        , (KebabCase, toKebabCase tokens)
        , (ConstantCase, toConstantCase tokens)
        ]

tokenizeCase :: Text -> [Text]
tokenizeCase txt =
    let spaced =
            T.concatMap
                ( \c ->
                    if c `elem` ("-_" :: String)
                        then " "
                        else
                            if isUpper c
                                then " " <> T.singleton c
                                else T.singleton c
                )
                txt
     in filter (not . T.null) $ T.words (T.toLower spaced)

toCamelCase, toLowerCamelCase, toKebabCase, toConstantCase :: [Text] -> Text
toCamelCase = T.concat . map capitalize
toLowerCamelCase [] = ""
toLowerCamelCase (x : xs) = x <> T.concat (map capitalize xs)
toKebabCase = T.intercalate "-"
toConstantCase = T.intercalate "_" . map T.toUpper

capitalize :: Text -> Text
capitalize t = case T.uncons t of
    Nothing -> ""
    Just (c, cs) -> (T.toUpper . T.singleton $ c) <> cs

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

escapeRegex :: Text -> Text
escapeRegex =
    T.concatMap
        ( \c ->
            if c `elem` ("\\^$.|?*+()[]{}" :: String)
                then "\\" <> T.singleton c
                else T.singleton c
        )
