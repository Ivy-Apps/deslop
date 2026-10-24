module Frontend.Haskell.IteratorSpec (spec) where

import Data.Text qualified as T
import Doubles.FileSystem (newInMemoryFileSystem, runInMemoryFileSystem)
import Effectful (runEff)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath)
import Fixtures.Frontend.Haskell.Project (inProject, projectRoot)
import Frontend.Haskell.Cabal (loadHsConfig)
import Frontend.Haskell.Ignore (hsEcosystemIgnored)
import Frontend.Haskell.Iterator (getHsFiles)
import Git.Ignore (loadGitIgnore)
import Test.Hspec

-- | The files discovered in a project holding these, by their path from the root.
discovered :: Text -> [Text] -> IO [Text]
discovered cabal = discoveredWith cabal . fmap (,"")

discoveredWith :: Text -> [(Text, Text)] -> IO [Text]
discoveredWith cabal files = do
    fs <- newInMemoryFileSystem $ (inProject "p.cabal", encodeUtf8 cabal) : [(inProject p, encodeUtf8 c) | (p, c) <- files]
    found <- runEff . runInMemoryFileSystem fs $ do
        loaded <- loadHsConfig projectRoot
        gitIgnore <- loadGitIgnore hsEcosystemIgnored projectRoot.path
        either (const (pure [])) (getHsFiles gitIgnore . fst) loaded
    pure $ mapMaybe (T.stripPrefix "/project/" . decodeOsPath . (.osPath)) found

cabalWith :: [Text] -> Text
cabalWith stanzas = T.unlines (["cabal-version: 3.0", "name: p", "version: 0"] <> stanzas)

spec :: Spec
spec = describe "Frontend.Haskell.Iterator" $ do
    it "finds the .hs files under every component's source directories" $
        discovered
            (cabalWith ["library\n  hs-source-dirs: src", "executable e\n  main-is: Main.hs\n  hs-source-dirs: app"])
            ["src/A.hs", "src/B/C.hs", "app/Main.hs", "scripts/Build.hs"]
            `shouldReturn` ["app/Main.hs", "src/A.hs", "src/B/C.hs"]

    it "finds a file once, however many source directories hold it" $
        discovered
            (cabalWith ["library\n  hs-source-dirs: .", "executable e\n  main-is: Main.hs\n  hs-source-dirs: src"])
            ["src/Main.hs"]
            `shouldReturn` ["src/Main.hs"]

    it "does not read boot files or literate Haskell" $
        discovered (cabalWith ["library\n  hs-source-dirs: src"]) ["src/A.hs", "src/A.hs-boot", "src/B.lhs"]
            `shouldReturn` ["src/A.hs"]

    it "does not walk what cabal and stack build into" $
        discovered (cabalWith ["library"]) ["A.hs", "dist-newstyle/build/Paths_p.hs", ".stack-work/B.hs"]
            `shouldReturn` ["A.hs"]

    it "does not walk what git ignores" $
        discoveredWith (cabalWith ["library"]) [("A.hs", ""), ("generated/B.hs", ""), (".gitignore", "generated/\n")]
            `shouldReturn` ["A.hs"]

    it "finds nothing in a source directory that does not exist" $
        discovered (cabalWith ["library\n  hs-source-dirs: missing"]) ["src/A.hs"] `shouldReturn` []
