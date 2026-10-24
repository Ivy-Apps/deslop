module Frontend.NixSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (problem)
import Doubles.FileSystem (defaultMockRoFileSystem, runInMemoryFileSystem, runReadOnly)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Fixtures.Deslop.Frontend (lowerProject)
import Fixtures.FileSystem.Path (inProject, projectFileSystem, projectRoot)
import Frontend.Nix (NixFrontend (..))
import Frontend.Nix.Reference (Scope (..))
import Generators.Frontend.Nix.Source (genSource, genTricky)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Renderable qualified
import Test.Hspec
import TestUtils (prop)

detectedIn :: [(Text, ByteString)] -> IO (Maybe NixFrontend)
detectedIn files = do
    fs <- projectFileSystem files
    runEff . runInMemoryFileSystem fs $ detect projectRoot

-- | Why loading these files fails, as the person running Deslop reads it.
configErrorOf :: [(Text, ByteString)] -> IO Text
configErrorOf files = either Renderable.render (const "loaded") <$> lowerProject NixFrontend files

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

    describe "loading the project" $
        it "refuses settings it cannot read, naming the file" $ do
            err <- configErrorOf [("flake.nix", "{ }"), ("deslop/nix.yaml", "roots: [root\n")]

            toString err `shouldStartWith` "Failed to read deslop/nix.yaml:\n"

    describe "parsing" $
        prop "renders a parsed file back to its bytes" $ do
            text <- forAll (Gen.choice [snd <$> genSource, genTricky])
            let bytes = encodeUtf8 text
            render NixFrontend (parse NixFrontend scope (inProject "a.nix") bytes) === bytes

    describe "linting" $
        it "finds nothing, having no built-in rules" $ do
            findings <- runReadOnly defaultMockRoFileSystem . runReader projectRoot $ lint NixFrontend scope (parse NixFrontend scope (inProject "a.nix") "import ./b.nix")

            fmap problem findings `shouldBe` []
