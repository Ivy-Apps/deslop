module Frontend.Nix.IteratorSpec (spec) where

import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (runEff)
import FileSystem.Path (ProjectRoot (..))
import Fixtures.FileSystem.Path (fromProject, projectFileSystem, projectRoot)
import Frontend.Nix.Ignore (nixEcosystemIgnored)
import Frontend.Nix.Iterator (getNixFiles)
import Git.Ignore (loadGitIgnore)
import Test.Hspec

-- | The files discovered in a project holding these, by their path from the root.
discovered :: [(Text, ByteString)] -> IO [Text]
discovered files = do
    fs <- projectFileSystem files
    found <- runEff . runInMemoryFileSystem fs $ do
        gitIgnore <- loadGitIgnore nixEcosystemIgnored projectRoot.path
        getNixFiles gitIgnore projectRoot
    pure $ fmap fromProject found

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
