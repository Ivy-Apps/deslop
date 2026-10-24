module Frontend.Nix.PathResolverSpec (spec) where

import Data.Text qualified as T
import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (runEff)
import Effects.FileSystem (runRoFileSystemIO)
import FileSystem.Path (ProjectRoot (..), absPathUnsafe, encodeOsPathString, portablePath, relativePathTo)
import Fixtures.FileSystem.Path (fromProject, inProject, projectFileSystem, projectRoot)
import Frontend.Nix.PathResolver (matchesSegment, reachedBy)
import Frontend.Nix.Reference (Piece (..))
import System.Directory (createDirectoryIfMissing, createDirectoryLink)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

files :: [Text]
files =
    [ "lib/default.nix"
    , "lib/agents.nix"
    , "hosts/a/configuration.nix"
    , "hosts/b/configuration.nix"
    , "hosts/b/disk.nix"
    , "skills/x/SKILL.md"
    , "skills/y/SKILL.md"
    , "secrets/common.yaml"
    ]

-- | What the pattern reaches from the directory, both spelled from the project root, @""@ for the root itself.
reached :: Text -> [[Piece]] -> IO [Text]
reached from pattern = do
    fs <- projectFileSystem [(p, "") | p <- files]
    let start = bool (inProject from) projectRoot.path (T.null from)
    found <- runEff . runInMemoryFileSystem fs $ reachedBy projectRoot start pattern
    pure $ fmap fromProject found

{- | What the pattern reaches from the root of a real project, @project@,
beside which @outside\/x.nix@ is; @project\/link@ links to @outside@ and
@project\/inner@ to @project\/lib@.
-}
reachedThroughLinks :: [[Piece]] -> IO [Text]
reachedThroughLinks pattern = withSystemTempDirectory "deslop-nix-links" $ \tmp -> do
    let project = tmp </> "project"
    createDirectoryIfMissing True (project </> "lib")
    createDirectoryIfMissing True (tmp </> "outside")
    writeFileBS (project </> "lib" </> "x.nix") ""
    writeFileBS (tmp </> "outside" </> "x.nix") ""
    createDirectoryLink (tmp </> "outside") (project </> "link")
    createDirectoryLink (project </> "lib") (project </> "inner")
    let root = ProjectRoot (absPathUnsafe (encodeOsPathString project))
    found <- runEff . runRoFileSystemIO $ reachedBy root root.path pattern
    pure $ fmap (portablePath . relativePathTo root) found

lit :: Text -> [Piece]
lit = pure . Literal

spec :: Spec
spec = describe "Frontend.Nix.PathResolver" $ do
    describe "a literal path" $ do
        it "reaches the file it names" $
            reached "lib" [lit ".", lit "agents.nix"] `shouldReturn` ["lib/agents.nix"]

        it "reaches the default.nix of a directory" $
            reached "" [lit "lib"] `shouldReturn` ["lib/default.nix"]

        it "reaches a directory without a default.nix itself" $
            reached "" [lit "skills", lit "x"] `shouldReturn` ["skills/x"]

        it "reaches up through a parent" $
            reached "hosts/a" [lit "..", lit "b", lit "disk.nix"] `shouldReturn` ["hosts/b/disk.nix"]

        it "reaches nothing that is not there" $
            reached "" [lit "gone.nix"] `shouldReturn` []

        it "reaches nothing outside the project" $
            reached "" [lit "..", lit "project", lit "..", lit "elsewhere.nix"] `shouldReturn` []

        it "reaches nothing a link takes outside the project" $
            reachedThroughLinks [lit "link", lit "x.nix"] `shouldReturn` []

        it "reaches through a link that stays inside the project" $
            reachedThroughLinks [lit "inner", lit "x.nix"] `shouldReturn` ["inner/x.nix"]

    describe "a pattern" $ do
        it "reaches every entry a computed segment can name" $
            reached "" [lit "hosts", [Hole], lit "configuration.nix"] `shouldReturn` ["hosts/a/configuration.nix", "hosts/b/configuration.nix"]

        it "reaches only what the literal part of a segment allows" $
            reached "secrets" [[Hole, Literal ".yaml"]] `shouldReturn` ["secrets/common.yaml"]

        it "reaches every directory a trailing computed segment names" $
            reached "" [lit "skills", [Hole]] `shouldReturn` ["skills/x", "skills/y"]

        it "reaches nothing when no entry matches" $
            reached "" [lit "hosts", [Hole], lit "missing.nix"] `shouldReturn` []

    describe "matching one segment" $ do
        it "matches a hole against anything, the empty name included" $
            fmap (matchesSegment [Hole]) ["", "a", "a.nix"] `shouldBe` [True, True, True]

        it "matches literal text exactly around a hole" $
            fmap (matchesSegment [Literal "x-", Hole, Literal ".nix"]) ["x-a.nix", "x-.nix", "y-a.nix", "x-a.nix.bak"] `shouldBe` [True, True, False, False]
