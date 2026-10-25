{- | What a run was asked to do.

The driver owns it: a language frontend never learns whether its findings will
be fixed, reported or accepted into the Baseline.
-}
module Deslop.Command (Command (..), Exploration (..), Opening (..)) where

import Git.SourceLink (GitRef)

data Command = CheckC | FixC | BaselineC | ExploreC Exploration
    deriving (Show, Eq)

-- | What @explore@ is asked beyond the project: only it reads git.
data Exploration = Exploration
    { diffBase :: GitRef
    -- ^ What the changed modules are measured against.
    , opening :: Opening
    }
    deriving (Show, Eq)

-- | Whether the written page is opened in a browser.
data Opening
    = -- | Opened when a person is there to see it: on a terminal, outside CI.
      OpenWhenAttended
    | -- | Never opened, as @--ci@ asks.
      NeverOpen
    deriving (Show, Eq)
