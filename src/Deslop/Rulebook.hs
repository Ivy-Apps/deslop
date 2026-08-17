{- | The domain model of a rulebook: valid, compiled, and ready for the hot
path.

Everything here has already been through "Deslop.Rulebook.Compiler". A value of
this type cannot carry a pattern that failed to compile, a clause naming a
variable its target never binds, or a clause whose polarity was chosen by its
caller - which is what lets "Deslop.RuleEnforcer" read it without a single
check of its own.

This module is deliberately free of IO and of the YAML shape. Reading rulebooks
off disk is "Deslop.Rulebook.Loader"; what a rulebook file looks like is
"Deslop.Rulebook.Dto".
-}
module Deslop.Rulebook (
    Rulebook (..),
    RulebookId (..),
    Rule (..),
    RuleId (..),
    ForbidsClause (..),
    AllowsClause (..),
    UsesClause (..),
    ExistsClause (..),
) where

import Data.Aeson (FromJSON)
import Deslop.GlobPlus (CompiledClausePattern, CompiledExcludePattern, CompiledTargetPattern)

newtype RulebookId = RulebookId Text
    deriving stock (Show, Eq, Ord)

data Rulebook = Rulebook
    { id :: RulebookId
    , name :: Text
    , description :: Text
    , rules :: [Rule]
    }
    deriving stock (Show)

data Rule = Rule
    { id :: RuleId
    , description :: Text
    , target :: CompiledTargetPattern
    , exclude :: Maybe (NonEmpty CompiledExcludePattern)
    , forbids :: Maybe (NonEmpty ForbidsClause)
    , allows :: Maybe (NonEmpty AllowsClause)
    , uses :: Maybe (NonEmpty UsesClause)
    , exists :: Maybe (NonEmpty ExistsClause)
    , example :: Maybe Text
    , fix :: Text
    }
    deriving stock (Show)

newtype RuleId = RuleId Text
    deriving stock (Show, Eq, Ord)
    deriving newtype (FromJSON)

data UsesClause = UsesImport
    { target :: CompiledClausePattern
    , transitive :: Bool
    }
    deriving stock (Show, Eq)

data ForbidsClause = ForbidsImport
    { target :: CompiledClausePattern
    , transitive :: Bool
    }
    deriving stock (Show, Eq)

newtype AllowsClause = AllowsImport
    { target :: CompiledClausePattern
    }
    deriving stock (Show, Eq)

newtype ExistsClause = ExistsModule
    { target :: CompiledClausePattern
    }
    deriving stock (Show, Eq)
