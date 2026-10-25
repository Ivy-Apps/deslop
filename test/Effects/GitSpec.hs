module Effects.GitSpec (spec) where

import Data.Set qualified as Set
import Effectful (runEff)
import Effects.Git (GitRef (..), NoDiff (..), gitChangedFiles, gitSourceLinks, runGitIO)
import FileSystem.Path (ProjectRoot (..))
import Git.SourceLink (Branch (..), DiffAnchors (..), Forge (..), Revision (..), SourceBase (..), SourceLinks (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec
import TestUtils (ap, rp)
import UnliftIO.Exception (bracket)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "Effects.Git" $ do
    it "names what the branch committed, what is uncommitted and what is untracked, inside the project" $
        withRepo $ \dir -> do
            -- Given: main holds two files; the branch changes one, then the work tree more
            write dir "app/a.ts" "a"
            write dir "app/b.ts" "b"
            write dir "outside.ts" "o"
            write dir ".gitignore" "*.log\n"
            commitAll dir "main"
            git dir ["checkout", "-q", "-b", "feature"]
            write dir "app/a.ts" "a2"
            write dir "outside.ts" "o2"
            commitAll dir "feature"
            write dir "app/b.ts" "b2"
            write dir "app/new.ts" "n"
            write dir "app/debug.log" "ignored"

            -- When
            changed <- changedIn (dir <> "/app") "main"

            -- Then
            changed `shouldBe` Right (Set.fromList [rp "a.ts", rp "b.ts", rp "new.ts"])

    it "measures from origin's copy of the base when the remote has one" $
        withRepo $ \dir -> do
            -- Given: origin/main has moved past the local main, and taken a.ts with it
            write dir "a.ts" "a"
            commitAll dir "first"
            write dir "a.ts" "a2"
            commitAll dir "second"
            git dir ["update-ref", "refs/remotes/origin/main", "HEAD"]
            git dir ["checkout", "-q", "-b", "feature"]
            git dir ["branch", "-q", "-f", "main", "HEAD~1"]
            write dir "b.ts" "b"

            -- When
            changed <- changedIn dir "main"

            -- Then
            changed `shouldBe` Right (Set.fromList [rp "b.ts"])

    it "has no diff against a base nobody has" $
        withRepo $ \dir -> do
            write dir "a.ts" "a"
            commitAll dir "first"

            changedIn dir "no-such-branch" `shouldReturn` Left (UnknownBase (GitRef "no-such-branch"))

    it "reads a ref spelled like an option as a ref, never as git's option" $
        withRepo $ \dir -> do
            write dir "a.ts" "a"
            commitAll dir "first"

            changedIn dir "--fork-point" `shouldReturn` Left (UnknownBase (GitRef "--fork-point"))

    it "reads the project's repository, whatever GIT_DIR and GIT_WORK_TREE it was started with" $
        withRepo $ \dir -> withRepo $ \elsewhere -> do
            -- Given: a hook-like environment pointing at another repository
            write dir "a.ts" "a"
            commitAll dir "first"
            write dir "b.ts" "b"
            write elsewhere "c.ts" "c"
            commitAll elsewhere "first"

            -- When
            changed <- withEnv [("GIT_DIR", elsewhere <> "/.git"), ("GIT_WORK_TREE", elsewhere)] $ changedIn dir "main"

            -- Then
            changed `shouldBe` Right (Set.fromList [rp "b.ts"])

    it "has no diff outside a git repository" $
        withSystemTempDirectory "deslop-git" $ \dir ->
            changedIn dir "main" `shouldReturn` Left NotARepository

    describe "gitSourceLinks" $ do
        it "links a project in a subfolder to its folder on the branch checked out" $
            withRepo $ \dir -> do
                appOnGitHubBranch dir

                -- When
                base <- sourceBaseIn (dir <> "/app")

                -- Then
                base `shouldBe` Just "https://github.com/o/r/blob/feature/x/app/"

        it "anchors the diff of a GitHub project at its folder in the repository" $
            withRepo $ \dir -> do
                appOnGitHubBranch dir

                -- When
                links <- sourceLinksIn (dir <> "/app")

                -- Then
                (.forge) <$> links `shouldBe` Just (GitHub DiffAnchors {repoPrefix = "app/"})

        it "reads the files at the branch checked out" $
            withRepo $ \dir -> do
                write dir "a.ts" "a"
                commitAll dir "first"
                git dir ["remote", "add", "origin", "git@github.com:o/r.git"]

                fmap (.revision) <$> sourceLinksIn dir `shouldReturn` Just (OnBranch (Branch "main"))

        it "links a detached checkout to its commit" $
            withRepo $ \dir -> do
                write dir "a.ts" "a"
                commitAll dir "first"
                git dir ["remote", "add", "origin", "https://gitlab.com/o/r.git"]
                git dir ["checkout", "-q", "--detach"]
                sha <- toText . filter (/= '\n') <$> gitOutput dir ["rev-parse", "HEAD"]

                links <- sourceLinksIn dir

                ((.source.url) <$> links, (.revision) <$> links)
                    `shouldBe` (Just ("https://gitlab.com/o/r/-/blob/" <> sha <> "/"), Just (Detached sha))

        it "has no link without an origin" $
            withRepo $ \dir -> do
                write dir "a.ts" "a"
                commitAll dir "first"

                sourceBaseIn dir `shouldReturn` Nothing

        it "has no link outside a git repository" $
            withSystemTempDirectory "deslop-git" $ \dir ->
                sourceBaseIn dir `shouldReturn` Nothing
  where
    sourceBaseIn dir = fmap (.source.url) <$> sourceLinksIn dir
    sourceLinksIn dir = runEff . runGitIO $ gitSourceLinks (ProjectRoot . ap $ toText dir)
    changedIn dir ref = runEff . runGitIO $ gitChangedFiles (ProjectRoot . ap $ toText dir) (GitRef ref)

-- | A repository on GitHub with its project in @app/@, on the branch @feature/x@.
appOnGitHubBranch :: FilePath -> IO ()
appOnGitHubBranch dir = do
    write dir "app/a.ts" "a"
    commitAll dir "first"
    git dir ["checkout", "-q", "-b", "feature/x"]
    git dir ["remote", "add", "origin", "git@github.com:o/r.git"]

withRepo :: (FilePath -> IO a) -> IO a
withRepo act = withSystemTempDirectory "deslop-git" $ \dir -> do
    git dir ["init", "-q", "-b", "main"]
    act dir

write :: FilePath -> FilePath -> Text -> IO ()
write dir path content = do
    createDirectoryIfMissing True (takeDirectory (dir <> "/" <> path))
    writeFileText (dir <> "/" <> path) content

commitAll :: FilePath -> String -> IO ()
commitAll dir message = do
    git dir ["add", "-A"]
    git dir ["-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false", "commit", "-q", "-m", message]

git :: FilePath -> [String] -> IO ()
git dir = void . gitOutput dir

gitOutput :: FilePath -> [String] -> IO String
gitOutput dir args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "git" (["-C", dir] <> args)) ""
    case code of
        ExitSuccess -> pure out
        ExitFailure _ -> out <$ expectationFailure ("git " <> show args <> " failed: " <> err)

-- | Runs with these environment variables set, restoring what they were after.
withEnv :: [(String, String)] -> IO a -> IO a
withEnv vars act = bracket (traverse save vars) (traverse_ restore) (const act)
  where
    save (name, value) = (name,) <$> lookupEnv name <* setEnv name value
    restore (name, saved) = maybe (unsetEnv name) (setEnv name) saved
