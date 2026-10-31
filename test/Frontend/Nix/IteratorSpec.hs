module Frontend.Nix.IteratorSpec (spec) where

import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (Eff, IOE, runEff)
import Effects.FileSystem (RoFileSystem, WrFileSystem)
import FileSystem.Path (AbsPath, ProjectRoot (..))
import Fixtures.FileSystem.Path (fromProject, projectFileSystem, projectRoot)
import Frontend.Nix.Ignore (nixEcosystemIgnored)
import Frontend.Nix.Iterator (getNixFiles, getOtherFiles)
import Git.Ignore (GitIgnore, loadGitIgnore)
import Test.Hspec

-- | The files a walk finds in a project holding these, by their path from the root.
foundBy :: (GitIgnore -> ProjectRoot -> Eff [WrFileSystem, RoFileSystem, IOE] [AbsPath]) -> [(Text, ByteString)] -> IO [Text]
foundBy walk files = do
    fs <- projectFileSystem files
    found <- runEff . runInMemoryFileSystem fs $ do
        gitIgnore <- loadGitIgnore nixEcosystemIgnored projectRoot.path
        walk gitIgnore projectRoot
    pure $ fmap fromProject found

discovered :: [(Text, ByteString)] -> IO [Text]
discovered = foundBy getNixFiles

spec :: Spec
spec = describe "Frontend.Nix.Iterator" $ do
    it "finds every .nix file of the project, and nothing else" $
        discovered [("flake.nix", ""), ("lib/default.nix", ""), ("secrets/a.yaml", ""), ("run.sh", "")]
            `shouldReturn` ["flake.nix", "lib/default.nix"]

    it "does not walk what nix build leaves" $
        discovered [("flake.nix", ""), ("result/share/x.nix", ""), ("result-dev/y.nix", "")]
            `shouldReturn` ["flake.nix"]

    it "does not walk what git ignores" $
        discovered [("flake.nix", ""), (".gitignore", "generated/\n"), ("generated/x.nix", "")]
            `shouldReturn` ["flake.nix"]

    it "finds every other file of the project, and no .nix file" $
        foundBy getOtherFiles [("flake.nix", ""), ("lib/default.nix", ""), ("secrets/a.yaml", ""), ("run.sh", "")]
            `shouldReturn` ["run.sh", "secrets/a.yaml"]

    it "does not list what git ignores, what nix build leaves or .git" $
        foundBy getOtherFiles [(".gitignore", "generated/\n"), ("generated/x.md", ""), ("result/x.md", ""), (".git/HEAD", ""), ("a.md", "")]
            `shouldReturn` [".gitignore", "a.md"]
