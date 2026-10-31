{- | Asking git about the checkout a project sits in.

Four questions the explorer asks: which files of the project differ from where
its branch started, for its Diff only view, where its files can be read on the
web, for its links to them, which pull request shows their diff, and what its
@origin@ is, to name the page. The interpreter runs the @git@ and @gh@ binaries
in the project directory; a test hands "Doubles.Git" the answer instead.
-}
module Effects.Git (
    Branch (..),
    Git (..),
    GitRef (..),
    NoDiff (..),
    NoPullRequest (..),
    Diff (..),
    gitDiff,
    gitOrigin,
    gitPullRequest,
    gitSourceLinks,
    runGitIO,
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, encodeOsPath, relativePathTo, withAbsBaseUnsafe)
import Git.SourceLink (Branch (..), Checkout (..), Diff (..), GitRef (..), PullRequest, Revision (..), SourceLinks, pullRequestFromGh, sourceLinksOf)
import System.Directory (findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

-- | Why there is no diff to show. Never fatal: the explorer is written without one.
data NoDiff
    = -- | No @git@ binary on the PATH.
      GitMissing
    | -- | The project is in no git work tree.
      NotARepository
    | -- | Neither @origin\/<ref>@ nor @<ref>@ names a commit, or it shares no history with @HEAD@.
      UnknownBase GitRef
    deriving stock (Show, Eq)

-- | Why there is no pull request to link to. Never fatal: the explorer is written without one.
data NoPullRequest
    = -- | @HEAD@ is detached: there is no branch to ask about.
      NotOnABranch
    | -- | No @gh@ binary on the PATH.
      GhMissing
    | -- | @gh@ found no pull request for the branch, or is not logged in.
      NoneFound
    | -- | @gh@ did not answer in time.
      GhTimedOut
    deriving stock (Show, Eq)

data Git :: Effect where
    DiffOf :: ProjectRoot -> GitRef -> Git m (Either NoDiff Diff)
    SourceLinksOf :: ProjectRoot -> Git m (Maybe SourceLinks)
    PullRequestOf :: ProjectRoot -> Branch -> Git m (Either NoPullRequest PullRequest)
    OriginOf :: ProjectRoot -> Git m (Maybe Text)

type instance DispatchOf Git = 'Dynamic

{- | The project's files that differ from where its branch left the base:
committed since @git merge-base <base> HEAD@, staged, unstaged or untracked.
The base is @origin\/<ref>@ when the remote has it, else @<ref>@, and the
'Diff' names which. Only files inside the project count, spelled from its root.
-}
gitDiff :: (Git :> es) => ProjectRoot -> GitRef -> Eff es (Either NoDiff Diff)
gitDiff root = send . DiffOf root

{- | Where the project's files can be read on the web: its @origin@ on GitHub
or GitLab, at the branch checked out, or the commit when none is. None outside
a repository, without an @origin@, or on any other host. On GitHub, also how
a file's diff is found in a pull request.
-}
gitSourceLinks :: (Git :> es) => ProjectRoot -> Eff es (Maybe SourceLinks)
gitSourceLinks = send . SourceLinksOf

{- | The pull request of a branch, as @gh@ finds it. A network call, so given
up on after 'ghTimeout': only the explorer asks it.
-}
gitPullRequest :: (Git :> es) => ProjectRoot -> Branch -> Eff es (Either NoPullRequest PullRequest)
gitPullRequest root = send . PullRequestOf root

-- | The @origin@ remote's URL, as configured. None outside a repository or without one.
gitOrigin :: (Git :> es) => ProjectRoot -> Eff es (Maybe Text)
gitOrigin = send . OriginOf

runGitIO :: (IOE :> es) => Eff (Git : es) a -> Eff es a
runGitIO = interpret $ \_ -> \case
    DiffOf root ref -> liftIO $ diffIO root ref
    SourceLinksOf root -> liftIO $ sourceLinksIO root
    PullRequestOf root branch -> liftIO $ pullRequestIO root branch
    OriginOf root -> liftIO $ originIO root

-- | How long @gh@ may take before the explorer is written without a pull request: 3s.
ghTimeout :: Int
ghTimeout = 3000000

-- Killed on timeout: the process is cleaned up with the call it runs in.
pullRequestIO :: ProjectRoot -> Branch -> IO (Either NoPullRequest PullRequest)
pullRequestIO root branch =
    timeout ghTimeout (runIn "gh" root ["pr", "view", branch.name, "--json", "number,url"]) <&> \case
        Nothing -> Left GhTimedOut
        Just CouldNotRun -> Left GhMissing
        Just Failed -> Left NoneFound
        Just (Succeeded out) -> maybeToRight NoneFound . pullRequestFromGh $ encodeUtf8 out

sourceLinksIO :: ProjectRoot -> IO (Maybe SourceLinks)
sourceLinksIO root = do
    origin <- originIO root
    revision <-
        git ["symbolic-ref", "--short", "-q", "HEAD"] >>= \case
            Succeeded branch -> pure . Just . OnBranch . Branch $ T.strip branch
            _detached -> fmap (Detached . T.strip) . output <$> git ["rev-parse", "HEAD"]
    prefix <- git ["rev-parse", "--show-prefix"]
    pure . (sourceLinksOf =<<) $ Checkout <$> origin <*> revision <*> (T.strip <$> output prefix)
  where
    git = gitIn root
    output = \case
        Succeeded out -> Just out
        _ -> Nothing

originIO :: ProjectRoot -> IO (Maybe Text)
originIO root =
    gitIn root ["remote", "get-url", "origin"] <&> \case
        Succeeded origin -> Just origin
        _ -> Nothing

diffIO :: ProjectRoot -> GitRef -> IO (Either NoDiff Diff)
diffIO root ref =
    git ["rev-parse", "--is-inside-work-tree"] >>= \case
        CouldNotRun -> pure (Left GitMissing)
        Failed -> pure (Left NotARepository)
        Succeeded _ -> do
            mergeBase <- firstMergeBase [GitRef ("origin/" <> ref.text), ref]
            case mergeBase of
                Nothing -> pure (Left (UnknownBase ref))
                Just (base, commit) -> do
                    committed <- git ["diff", "--name-only", "--relative", "-z", T.strip commit]
                    untracked <- git ["ls-files", "--others", "--exclude-standard", "-z"]
                    pure . Right . Diff base . Set.fromList . fmap inProject $ foldMap paths [committed, untracked]
  where
    git = gitIn root

    -- The first base git knows, with the commit the branch left it at.
    firstMergeBase = \case
        [] -> pure Nothing
        base : rest ->
            git ["merge-base", "--end-of-options", base.text, "HEAD"] >>= \case
                Succeeded commit -> pure $ Just (base, commit)
                _ -> firstMergeBase rest

    paths = \case
        Succeeded out -> filter (not . T.null) (T.splitOn "\0" out)
        _ -> []

    inProject = relativePathTo root . withAbsBaseUnsafe root.path . encodeOsPath

-- | Runs @git@ in the project directory.
gitIn :: ProjectRoot -> [Text] -> IO Run
gitIn = runIn "git"

{- | Runs a program in the project directory. It finds the repository from
there, whatever @GIT_DIR@ or @GIT_WORK_TREE@ the caller was started with - as
a git hook is.
-}
runIn :: FilePath -> ProjectRoot -> [Text] -> IO Run
runIn program root args =
    findExecutable program >>= \case
        Nothing -> pure CouldNotRun
        Just executable -> do
            env <- filter ((`notElem` repositoryOverrides) . fst) <$> getEnvironment
            readCreateProcessWithExitCode
                (proc executable (toString <$> args))
                    { cwd = Just (toString . decodeOsPath $ root.path.osPath)
                    , env = Just env
                    }
                ""
                <&> \case
                    (ExitSuccess, out, _) -> Succeeded (toText out)
                    (ExitFailure _, _, _) -> Failed
  where
    repositoryOverrides = ["GIT_DIR", "GIT_WORK_TREE"]

-- | How one invocation went.
data Run = CouldNotRun | Failed | Succeeded Text
