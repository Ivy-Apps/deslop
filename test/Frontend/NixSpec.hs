module Frontend.NixSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (problem)
import Deslop.Module (Module (..))
import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, mockTree, runInMemoryFileSystem, runMockRoFileSystem, runReadOnly)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static (runReader)
import FileSystem.Path (AbsPath, portablePath)
import Fixtures.Deslop.Frontend (Lowered (..), lowerProjectAt)
import Fixtures.FileSystem.Path (inProject, projectFileSystem, projectRoot)
import Frontend.Nix (NixFrontend (..))
import Frontend.Nix.Reference (Scope (..))
import Generators.Frontend.Nix.Source (genSource, genTricky)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Renderable qualified
import Test.Hspec
import TestUtils (prop, requireRight)

-- | The modules of a project on a file system on which reading these paths fails, by their path.
modulesReadingNone :: [AbsPath] -> [(AbsPath, ByteString)] -> IO [Text]
modulesReadingNone unreadable files =
    fmap (sort . fmap (portablePath . (.path)) . (.modules))
        . requireRight (toString . Renderable.render)
        <=< runEff . runConcurrent . runMockRoFileSystem failingOnThem . runReader projectRoot
        $ lowerProjectAt NixFrontend projectRoot
  where
    tree = mockTree files
    failingOnThem = tree {mockReadFile = \path -> if path `elem` unreadable then error "read a file that must not be read" else tree.mockReadFile path}

detectedIn :: [(Text, ByteString)] -> IO (Maybe NixFrontend)
detectedIn files = do
    fs <- projectFileSystem files
    runEff . runInMemoryFileSystem fs $ detect projectRoot

scope :: Scope
scope = Scope {roots = one "self", inputs = mempty}

spec :: Spec
spec = describe "Frontend.Nix" $ do
    describe "detection" $ do
        for_
            [ ("a flake.nix at its root", [("flake.nix", "{ }")])
            , ("a default.nix at its root", [("default.nix", "{ }")])
            , ("a flake.nix it cannot read", [("flake.nix", "\"")])
            ]
            $ \(what, files) ->
                it ("recognises a project with " <> what) $
                    detectedIn files `shouldReturn` Just NixFrontend

        it "does not recognise Nix files below the root" $
            detectedIn [("nix/default.nix", "{ }"), ("shell.nix", "{ }")] `shouldReturn` Nothing

    describe "parsing" $
        prop "renders a parsed file back to its bytes" $ do
            text <- forAll (Gen.choice [snd <$> genSource, genTricky])
            let bytes = encodeUtf8 text
            render NixFrontend (parse NixFrontend scope (inProject "a.nix") bytes) === bytes

    describe "linting" $
        it "finds nothing, having no built-in rules" $ do
            findings <- runReadOnly defaultMockRoFileSystem . runReader projectRoot $ lint NixFrontend scope (parse NixFrontend scope (inProject "a.nix") "import ./b.nix")

            fmap problem findings `shouldBe` []

    let at path content = (inProject path, content)

    describe "lowering" $
        it "lists a file that is not Nix without ever reading it" $ do
            let unreadable = fmap inProject ["docs/huge.md", "tool.py", "logo.bin"]

            modulesReadingNone unreadable (("flake.nix" `at` "{ }") : fmap (,"") unreadable)
                `shouldReturn` ["docs/huge.md", "flake.nix", "logo.bin", "tool.py"]
