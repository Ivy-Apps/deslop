module Frontend.Nix.ConfigSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Error (Warning (..))
import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (runEff)
import Fixtures.FileSystem.Path (projectFileSystem, projectRoot)
import Frontend.Nix.Config (NixConfigError (..), loadNixConfig)
import Frontend.Nix.Reference (Scope (..))
import Renderable qualified
import Test.Hspec

loaded :: [(Text, ByteString)] -> IO (Either NixConfigError (Scope, [Warning]))
loaded files = do
    fs <- projectFileSystem files
    runEff . runInMemoryFileSystem fs $ loadNixConfig projectRoot

rootsOf :: [(Text, ByteString)] -> IO (Either NixConfigError [Text])
rootsOf = fmap (fmap (Set.toList . (.roots) . fst)) . loaded

-- | The inputs read, and the warnings given, as the person running Deslop reads them.
inputsOf :: [(Text, ByteString)] -> IO (Either NixConfigError ([Text], [Text]))
inputsOf = fmap (fmap (\(s, ws) -> (Set.toList s.inputs, fmap (\(Warning w) -> w) ws))) . loaded

-- | Why loading these settings fails, as the person running Deslop reads it.
refusalOf :: ByteString -> IO (Maybe Text)
refusalOf settings = fmap Renderable.render . leftToMaybe <$> rootsOf [("deslop/nix.yaml", settings)]

spec :: Spec
spec = describe "Frontend.Nix.Config" $ do
    describe "roots" $ do
        it "names the root self when there are no settings" $
            rootsOf [("flake.nix", "{ }")] `shouldReturn` Right ["self"]

        it "adds the roots the settings name to self" $
            rootsOf [("flake.nix", "{ }"), ("deslop/nix.yaml", "roots: [root, src]\n")] `shouldReturn` Right ["root", "self", "src"]

        it "names the root self when the settings name none" $
            rootsOf [("deslop/nix.yaml", "{}\n")] `shouldReturn` Right ["self"]

        it "refuses settings that are not YAML, naming the file" $ do
            refusal <- refusalOf "roots: [root\n"

            refusal `shouldSatisfy` maybe False ("Failed to read deslop/nix.yaml:\n" `T.isPrefixOf`)

        it "refuses a key it does not know, naming the file and the key" $ do
            refusal <- refusalOf "root: [root]\n"

            refusal `shouldSatisfy` maybe False (\t -> "Failed to read deslop/nix.yaml:\n" `T.isPrefixOf` t && "root" `T.isInfixOf` t)

        it "refuses a root that is no Nix identifier" $
            rootsOf [("deslop/nix.yaml", "roots: [\"my root\"]\n")] `shouldReturn` Left (RootNotAnIdentifier "my root")

    describe "inputs" $ do
        it "reads the inputs the root flake.nix declares" $
            inputsOf [("flake.nix", "{ inputs.nixpkgs.url = \"github:NixOS/nixpkgs\"; outputs = _: { }; }")]
                `shouldReturn` Right (["nixpkgs"], [])

        it "reads none without a flake.nix" $
            inputsOf [("default.nix", "{ }")] `shouldReturn` Right ([], [])

        it "warns of a flake.nix it cannot read, and reads no inputs" $
            inputsOf [("flake.nix", "{ inputs = \"")]
                `shouldReturn` Right ([], ["flake.nix could not be read, so no reference is read as a flake input."])
