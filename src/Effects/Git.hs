{- | Asking git about the checkout a project sits in.

Three questions the diagram asks: which files of the project differ from where
its branch started, for its Diff only view, where its files can be read on the
web, for its links to them, and which pull request shows their diff. The
interpreter runs the @git@ and @gh@ binaries in the project directory; a test
hands "Doubles.Git" the answer instead.
-}
module Effects.Git (
    Git (..),
    GitRef (..),
    NoDiff (..),
    NoPullRequest (..),
    gitChangedFiles,
    gitPullRequest,
    gitSourceLinks,
    runGitIO,
) where

import Control.Exception (IOException, try)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import FileSystem.Path (AbsPath (..), ProjectRelativePath, ProjectRoot (..), decodeOsPath, encodeOsPath, relativePathTo, withAbsBaseUnsafe)
import Git.SourceLink (Checkout (..), GitRef (..), PullRequest, SourceLinks, pullRequestFromGh, sourceLinksOf)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

-- | Why there is no diff to show. Never fatal: the diagram is drawn without one.
data NoDiff
    = -- | No @git@ binary on the PATH.
      GitMissing
    | -- | The project is in no git work tree.
      NotARepository
    | -- | Neither @origin\/<ref>@ nor @<ref>@ names a commit, or it shares no history with @HEAD@.
      UnknownBase GitRef
    deriving stock (Show, Eq)

-- | Why there is no pull request to link to. Never fatal: the diagram is drawn without one.
data NoPullRequest
    = -- | @HEAD@ is detached, or the project is in no git work tree.
      NotOnABranch
    | -- | No @gh@ binary on the PATH.
      GhMissing
    | -- | @gh@ found no pull request for the branch, or is not logged in.
      NoneFound
    | -- | @gh@ did not answer in time.
      GhTimedOut
    deriving stock (Show, Eq)

data Git :: Effect where
    ChangedFiles :: ProjectRoot -> GitRef -> Git m (Either NoDiff (Set ProjectRelativePath))
    SourceLinksOf :: ProjectRoot -> Git m (Maybe SourceLinks)
    PullRequestOf :: ProjectRoot -> Git m (Either NoPullRequest PullRequest)

type instance DispatchOf Git = 'Dynamic

{- | The project's files that differ from where its branch left the base:
committed since @git merge-base <base> HEAD@, staged, unstaged or untracked.
The base is @origin\/<ref>@ when the remote has it, else @<ref>@. Only files
inside the project count, spelled from its root.
-}
gitChangedFiles :: (Git :> es) => ProjectRoot -> GitRef -> Eff es (Either NoDiff (Set ProjectRelativePath))
gitChangedFiles root = send . ChangedFiles root

{- | Where the project's files can be read on the web: its @origin@ on GitHub
or GitLab, at the branch checked out, or the commit when none is. None outside
a repository, without an @origin@, or on any other host. On GitHub, also how
a file's diff is found in a pull request.
-}
gitSourceLinks :: (Git :> es) => ProjectRoot -> Eff es (Maybe SourceLinks)
gitSourceLinks = send . SourceLinksOf

{- | The pull request of the branch checked out, as @gh@ finds it. A network
call, so given up on after 'ghTimeout': only the diagram asks it.
-}
gitPullRequest :: (Git :> es) => ProjectRoot -> Eff es (Either NoPullRequest PullRequest)
gitPullRequest = send . PullRequestOf

runGitIO :: (IOE :> es) => Eff (Git : es) a -> Eff es a
runGitIO = interpret $ \_ -> \case
    ChangedFiles root ref -> liftIO $ changedFilesIO root ref
    SourceLinksOf root -> liftIO $ sourceLinksIO root
    PullRequestOf root -> liftIO $ pullRequestIO root

-- | How long @gh@ may take before the diagram is drawn without a pull request: 3s.
ghTimeout :: Int
ghTimeout = 3000000

pullRequestIO :: ProjectRoot -> IO (Either NoPullRequest PullRequest)
pullRequestIO root =
    gitIn root ["symbolic-ref", "--short", "-q", "HEAD"] >>= \case
        Succeeded branch -> ghPullRequest (T.strip branch)
        _ -> pure (Left NotOnABranch)
  where
    -- Killed on timeout: the process is cleaned up with the call it runs in.
    ghPullRequest branch =
        timeout ghTimeout (runIn "gh" root ["pr", "view", branch, "--json", "number,url"]) <&> \case
            Nothing -> Left GhTimedOut
            Just CouldNotRun -> Left GhMissing
            Just Failed -> Left NoneFound
            Just (Succeeded out) -> maybeToRight NoneFound . pullRequestFromGh $ encodeUtf8 out

sourceLinksIO :: ProjectRoot -> IO (Maybe SourceLinks)
sourceLinksIO root = do
    origin <- git ["remote", "get-url", "origin"]
    branch <- git ["symbolic-ref", "--short", "-q", "HEAD"]
    revision <- case branch of
        Succeeded _ -> pure branch
        _ -> git ["rev-parse", "HEAD"]
    prefix <- git ["rev-parse", "--show-prefix"]
    pure . (sourceLinksOf =<<) $ Checkout <$> output origin <*> (T.strip <$> output revision) <*> (T.strip <$> output prefix)
  where
    git = gitIn root
    output = \case
        Succeeded out -> Just out
        _ -> Nothing

changedFilesIO :: ProjectRoot -> GitRef -> IO (Either NoDiff (Set ProjectRelativePath))
changedFilesIO root ref =
    git ["rev-parse", "--is-inside-work-tree"] >>= \case
        CouldNotRun -> pure (Left GitMissing)
        Failed -> pure (Left NotARepository)
        Succeeded _ -> do
            mergeBase <- firstSuccess [git ["merge-base", base, "HEAD"] | base <- ["origin/" <> ref.text, ref.text]]
            case mergeBase of
                Nothing -> pure (Left (UnknownBase ref))
                Just commit -> do
                    committed <- git ["diff", "--name-only", "--relative", "-z", T.strip commit]
                    untracked <- git ["ls-files", "--others", "--exclude-standard", "-z"]
                    pure . Right . Set.fromList . fmap inProject $ foldMap paths [committed, untracked]
  where
    git = gitIn root

    firstSuccess = \case
        [] -> pure Nothing
        run : rest ->
            run >>= \case
                Succeeded out -> pure (Just out)
                _ -> firstSuccess rest

    paths = \case
        Succeeded out -> filter (not . T.null) (T.splitOn "\0" out)
        _ -> []

    inProject = relativePathTo root . withAbsBaseUnsafe root.path . encodeOsPath

-- | Runs @git@ in the project directory.
gitIn :: ProjectRoot -> [Text] -> IO Run
gitIn = runIn "git"

-- | Runs a program in the project directory.
runIn :: FilePath -> ProjectRoot -> [Text] -> IO Run
runIn program root args = do
    result <- try @IOException $ readCreateProcessWithExitCode (proc program (toString <$> args)) {cwd = Just (toString . decodeOsPath $ root.path.osPath)} ""
    pure $ case result of
        Left _ -> CouldNotRun
        Right (ExitSuccess, out, _) -> Succeeded (toText out)
        Right (ExitFailure _, _, _) -> Failed

-- | How one invocation went.
data Run = CouldNotRun | Failed | Succeeded Text
