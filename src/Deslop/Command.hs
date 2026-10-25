{- | What a run was asked to do.

The driver owns it: a language frontend never learns whether its findings will
be fixed, reported or accepted into the Baseline.
-}
module Deslop.Command (Command (..), Exploration (..)) where

import Git.SourceLink (GitRef)

data Command = CheckC | FixC | BaselineC | ExploreC Exploration
    deriving (Show, Eq)

-- | What @explore@ is asked beyond the project: only it reads git.
newtype Exploration = Exploration
    { diffBase :: GitRef
    -- ^ What the changed modules are measured against.
    }
    deriving (Show, Eq)
