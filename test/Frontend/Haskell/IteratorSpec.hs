module Frontend.Haskell.IteratorSpec (spec) where

import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (runEff)
import FileSystem.Path (ProjectRoot (..))
import Fixtures.FileSystem.Path (fromProject, projectFileSystem)
import Fixtures.Frontend.Haskell.Project (cabalPackage, projectRoot)
import Frontend.Haskell.Cabal (HsConfig (..), loadHsConfig)
import Frontend.Haskell.Ignore (hsEcosystemIgnored)
import Frontend.Haskell.Iterator (getHsFiles)
import Git.Ignore (loadGitIgnore)
import Test.Hspec

-- | The files discovered in a project holding these, by their path from the root.
discovered :: Text -> [Text] -> IO [Text]
discovered cabal = discoveredWith cabal . fmap (,"")

discoveredWith :: Text -> [(Text, Text)] -> IO [Text]
discoveredWith cabal files = do
    fs <- projectFileSystem $ ("p.cabal", encodeUtf8 cabal) : fmap (second encodeUtf8) files
    found <- runEff . runInMemoryFileSystem fs $ do
        loaded <- loadHsConfig projectRoot
        gitIgnore <- loadGitIgnore hsEcosystemIgnored projectRoot.path
        either (const (pure [])) (getHsFiles gitIgnore . (.components) . fst) loaded
    pure $ fmap fromProject found

spec :: Spec
spec = describe "Frontend.Haskell.Iterator" $ do
    it "finds the .hs files under every component's source directories" $
        discovered
            (cabalPackage ["library\n  hs-source-dirs: src", "executable e\n  main-is: Main.hs\n  hs-source-dirs: app"])
            ["src/A.hs", "src/B/C.hs", "app/Main.hs", "scripts/Build.hs"]
            `shouldReturn` ["app/Main.hs", "src/A.hs", "src/B/C.hs"]

    it "finds a file once, however many source directories hold it" $
        discovered
            (cabalPackage ["library\n  hs-source-dirs: .", "executable e\n  main-is: Main.hs\n  hs-source-dirs: src"])
            ["src/Main.hs"]
            `shouldReturn` ["src/Main.hs"]

    it "does not read boot files or literate Haskell" $
        discovered (cabalPackage ["library\n  hs-source-dirs: src"]) ["src/A.hs", "src/A.hs-boot", "src/B.lhs"]
            `shouldReturn` ["src/A.hs"]

    it "does not walk what cabal and stack build into" $
        discovered (cabalPackage ["library"]) ["A.hs", "dist-newstyle/build/Paths_p.hs", ".stack-work/B.hs"]
            `shouldReturn` ["A.hs"]

    it "does not walk what git ignores" $
        discoveredWith (cabalPackage ["library"]) [("A.hs", ""), ("generated/B.hs", ""), (".gitignore", "generated/\n")]
            `shouldReturn` ["A.hs"]

    it "finds nothing in a source directory that does not exist" $
        discovered (cabalPackage ["library\n  hs-source-dirs: missing"]) ["src/A.hs"] `shouldReturn` []
