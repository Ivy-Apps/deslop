{- | What a run resolves a project's TypeScript configuration to, and how the
files it was spread across are merged into it.

A 'TsConfig' is not a file: it is the /effective/ configuration, after every
@extends@ in the chain has been folded in. One file's contribution is a
'Declared', and merging the chain is that type's 'Semigroup' - right-biased, so
a config always outranks the ones it extends. Reading the files and ordering
them is "TypeScript.Config.Loader".
-}
module TypeScript.Config (
    TsConfig (..),
    Declared (..),
    DeclaredPaths (..),
    effectiveConfig,
    pathMappings,
    parsePattern,
    parsePathMapping,
    PathMapping (..),
    Pattern (..),
    KeyPattern (..),
    ValuePattern (..),
) where

import Data.Map qualified as M
import Data.Text qualified as T
import FileSystem.Path (AbsPath)

data TsConfig = TsConfig
    { pathsBase :: !AbsPath
    -- ^ The directory a 'ValuePattern' resolves against.
    , paths :: ![PathMapping]
    }
    deriving (Show, Eq)

{- | What one config file states for itself, with its own directory already
baked in, so that merging never has to ask where a value came from.

Every field is a 'Last' because that /is/ the merge rule: TypeScript overlays
@compilerOptions@ key by key, and a key a config declares replaces the
inherited one outright. @paths@ in particular is one option value, not a map to
be unioned - a config that declares any @paths@ discards its base's entirely.
-}
data Declared = Declared
    { baseUrl :: !(Last AbsPath)
    , paths :: !(Last DeclaredPaths)
    }
    deriving (Show, Eq)

-- | Path mappings, together with the directory of the config that declared them.
data DeclaredPaths = DeclaredPaths
    { base :: !AbsPath
    , mappings :: ![PathMapping]
    }
    deriving (Show, Eq)

instance Semigroup Declared where
    a <> b =
        Declared
            { baseUrl = a.baseUrl <> b.baseUrl
            , paths = a.paths <> b.paths
            }

instance Monoid Declared where
    mempty = Declared {baseUrl = mempty, paths = mempty}

{- | The configuration a merged chain amounts to.

The base that path mappings resolve against is a declared @baseUrl@ wherever
the chain has one; failing that, the directory of the config that declared the
winning @paths@, which is what the compiler calls the paths base path; failing
even that, the directory of the config the chain was rooted at.
-}
effectiveConfig :: AbsPath -> Declared -> TsConfig
effectiveConfig rootDir declared = case (getLast declared.baseUrl, getLast declared.paths) of
    (Just base, Just ps) -> TsConfig {pathsBase = base, paths = ps.mappings}
    (Just base, Nothing) -> TsConfig {pathsBase = base, paths = []}
    (Nothing, Just ps) -> TsConfig {pathsBase = ps.base, paths = ps.mappings}
    (Nothing, Nothing) -> TsConfig {pathsBase = rootDir, paths = []}

{- | The @paths@ of one config, in the order the resolver must try them.

Unparseable mappings are dropped rather than reported: a @paths@ entry Deslop
cannot read is one alias it cannot resolve, not a reason to refuse the project.
-}
pathMappings :: Map Text [Text] -> [PathMapping]
pathMappings = sortPathMappings . mapMaybe parsePathMapping . M.toList

sortPathMappings :: [PathMapping] -> [PathMapping]
sortPathMappings = sortOn (Down . patternSortKey . extractPattern . (.key))
  where
    extractPattern :: KeyPattern -> Pattern
    extractPattern (KeyPattern p) = p
    -- 'Down' reverses the default ascending sort, meaning higher numbers come first.
    patternSortKey :: Pattern -> (Int, Int, Int)
    patternSortKey (Exact k) =
        -- Priority 1: Exact matches always float to the top.
        (1, T.length k, 0)
    patternSortKey (Wildcard pre suff) =
        -- Priority 0: Wildcards come after Exact matches.
        -- They are sub-sorted by prefix length, then suffix length.
        (0, T.length pre, T.length suff)

data PathMapping = PathMapping
    { key :: !KeyPattern
    , values :: !(NonEmpty ValuePattern)
    }
    deriving (Show, Eq)

newtype KeyPattern = KeyPattern
    { pattern :: Pattern
    }
    deriving (Show, Eq)

newtype ValuePattern = ValuePattern
    { pattern :: Pattern
    }
    deriving (Show, Eq)

data Pattern
    = Exact !Text
    | Wildcard {pre :: !Text, suff :: !Text}
    deriving (Show, Eq)

parsePathMapping :: (Text, [Text]) -> Maybe PathMapping
parsePathMapping (_, []) = Nothing
parsePathMapping (k, vs) = do
    key <- parsePattern k
    values <-
        nonEmpty
            . fmap cleanValuePattern
            . filter (validKeyValuePair key)
            . mapMaybe parsePattern
            $ vs
    Just
        PathMapping
            { key = KeyPattern key
            , values = ValuePattern <$> values
            }
  where
    cleanValuePattern :: Pattern -> Pattern
    cleanValuePattern (Exact t) = Exact (cleanPrefix t)
    cleanValuePattern (Wildcard pre suff) = Wildcard (cleanPrefix pre) suff

    cleanPrefix :: Text -> Text
    cleanPrefix t
        | t == "." = ""
        | Just rest <- T.stripPrefix "./" t = cleanPrefix rest
        | otherwise = t

    validKeyValuePair :: Pattern -> Pattern -> Bool
    validKeyValuePair (Exact _) (Wildcard _ _) = False
    validKeyValuePair _ _ = True

parsePattern :: Text -> Maybe Pattern
parsePattern "" = Nothing
parsePattern t = case T.count "*" t of
    0 -> Just $ Exact t
    1 -> let (pre, suff) = T.breakOn "*" t in Just $ Wildcard pre (T.drop 1 suff)
    _ -> Nothing
