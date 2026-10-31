{- | A repository held in memory: what has changed since each base ref it
knows, where its files can be read on the web, its pull request and its
@origin@. A ref is looked up as @origin\/<ref>@ first, then as itself, as git
is asked; any other ref is unknown, as it is to git.

Each answer is forced when asked for, so a field left 'error' fails a run that
asks it.
-}
module Doubles.Git (
    Repository (..),
    runGitInMemory,
    noChanges,
) where

import Data.Map.Strict qualified as Map
import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects.Git (Diff (..), Git (..), GitRef (..), NoDiff (..), NoPullRequest (..))
import FileSystem.Path (ProjectRelativePath)
import Git.SourceLink (PullRequest, SourceLinks)

data Repository = Repository
    { changes :: Map GitRef (Set ProjectRelativePath)
    , sourceLinks :: Maybe SourceLinks
    , pullRequest :: Either NoPullRequest PullRequest
    , origin :: Maybe Text
    }

runGitInMemory :: Repository -> Eff (Git : es) a -> Eff es a
runGitInMemory repo = interpret $ \_ -> \case
    DiffOf _ ref ->
        pure $!
            maybeToRight (UnknownBase ref) . viaNonEmpty head $
                [Diff base files | base <- [GitRef ("origin/" <> ref.text), ref], Just files <- [Map.lookup base repo.changes]]
    SourceLinksOf _ -> pure $! repo.sourceLinks
    PullRequestOf _ _ -> pure $! repo.pullRequest
    OriginOf _ -> pure $! repo.origin

-- | A repository whose @origin/main@ is where the work tree stands, with no remote.
noChanges :: Repository
noChanges = Repository {changes = Map.singleton (GitRef "origin/main") mempty, sourceLinks = Nothing, pullRequest = Left NoneFound, origin = Nothing}
