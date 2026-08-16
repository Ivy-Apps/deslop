module Types (
    DeslopError (..),
    ModuleCount (..),
    ProblemCounts (..),
    RuleCount (..),
    RunReport (..),
    RunSummary (..),
    Verdict (..),
    Renderable (..),
) where

import System.OsPath (OsPath)

class Renderable a where
    render :: a -> Text

instance (Renderable a) => Renderable [a] where
    render = foldl' (\acc x -> acc <> render x) ""

data DeslopError
    = TsConfigNotFoundError OsPath
    | TsConfigParseError Text
    | RulebookError Text
    | InvalidRuleConfig Text
    deriving (Show, Eq)

-- | What a run that reached the end has to say for itself.
data RunReport = RunReport
    { summary :: RunSummary
    , verdict :: Verdict
    }
    deriving (Show, Eq)

{- | What a run covered, per command. Only the commands that enforce Rulebook
Rules carry a 'RuleCount'; @fix@ enforces none, so it cannot claim any.
-}
data RunSummary
    = Checked ModuleCount RuleCount
    | Baselined ModuleCount RuleCount
    | Scanned ModuleCount
    deriving (Show, Eq)

-- | How many modules a run went through.
newtype ModuleCount = ModuleCount Int
    deriving stock (Show, Eq)

-- | How many Rulebook Rules a run enforced.
newtype RuleCount = RuleCount Int
    deriving stock (Show, Eq)

-- | Whether a run found anything the user must act on.
data Verdict
    = Clean
    | ProblemsFound ProblemCounts
    deriving (Show, Eq)

data ProblemCounts = ProblemCounts
    { total :: Int
    , autoFixable :: Int
    }
    deriving (Show, Eq)
