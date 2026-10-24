{- | Glob+ patterns compiled the way a rulebook compiles them, and matched
against whole paths, for the specs about what a pattern means.

The matchers take a path already split into segments, because production
splits each module id once and reuses it. The specs are about matching rather
than about that, so they hand over a whole path and let these split it.
-}
module Fixtures.Deslop.GlobPlus (
    targetPattern,
    clausePattern,
    matchTargetAt,
    matchClauseAt,
    braced,
) where

import Deslop.GlobPlus (
    CompiledClausePattern,
    CompiledTargetPattern,
    Determinism (..),
    MatchEnv,
    Polarity,
    VarName,
    matchClause,
    matchTarget,
    segmentsOf,
 )
import Deslop.GlobPlus.Compiler (compileClausePattern, compileTargetPattern, renderGlobPlusError)

-- | A target pattern the spec relies on compiling.
targetPattern :: (HasCallStack) => Text -> CompiledTargetPattern
targetPattern t = case compileTargetPattern t of
    Right compiled -> compiled
    Left err -> error $ "Failed to compile target pattern: " <> renderGlobPlusError err

{- | A clause pattern the spec relies on compiling, of the polarity and in the
scope of the variables its target binds. Clauses that must name exactly one
module are compiled by the specs about that.
-}
clausePattern :: (HasCallStack) => Polarity -> Set VarName -> Text -> CompiledClausePattern
clausePattern polarity bound t = case compileClausePattern polarity Nondeterministic bound t of
    Right compiled -> compiled
    Left err -> error $ "Failed to compile clause pattern: " <> renderGlobPlusError err

matchTargetAt :: CompiledTargetPattern -> Text -> Maybe MatchEnv
matchTargetAt target = matchTarget target . segmentsOf

matchClauseAt :: CompiledClausePattern -> MatchEnv -> Text -> Bool
matchClauseAt clause env = matchClause clause env . segmentsOf

-- | A variable token, e.g. @braced "FileName"@ is @{{FileName}}@.
braced :: Text -> Text
braced t = "{{" <> t <> "}}"
