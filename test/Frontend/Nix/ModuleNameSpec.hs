module Frontend.Nix.ModuleNameSpec (spec) where

import Data.Text qualified as T
import Deslop.Module (ModuleName (..))
import Frontend.Nix.ModuleName (externalName, moduleNames)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop, rp)

namesOf :: Text -> [Text]
namesOf = fmap (.text) . toList . moduleNames . rp

-- | A path from the project root, where Nix files, their directories and what is not Nix meet.
genPath :: Gen Text
genPath = do
    directories <- Gen.list (Range.linear 0 2) segment
    file <- segment
    extension <- Gen.element [".nix", "", ".yaml", ".yaml.nix"]
    pure . T.intercalate "/" $ directories <> [file <> extension]
  where
    segment = Gen.element ["a", "b", "default", "flake"]

spec :: Spec
spec = describe "Frontend.Nix.ModuleName" $ do
    it "names a Nix file by its path, with the extension" $
        namesOf "modules/home/git.nix" `shouldBe` ["modules/home/git.nix"]

    it "names a default.nix by its path, aliased by its directory" $
        namesOf "lib/default.nix" `shouldBe` ["lib/default.nix", "lib"]

    it "gives the root's default.nix no alias" $
        namesOf "default.nix" `shouldBe` ["default.nix"]

    it "names what is not Nix by its path" $
        namesOf "secrets/common.yaml" `shouldBe` ["secrets/common.yaml"]

    it "names a directory by its path" $
        namesOf "programs/agents/skills" `shouldBe` ["programs/agents/skills"]

    it "names a module outside the project by its segments, with the extension" $
        fmap (.text) [externalName ("nixpkgs" :| []), externalName ("nixpkgs" :| ["lib", "default.nix"])]
            `shouldBe` ["nixpkgs", "nixpkgs/lib/default.nix"]

    prop "calls every path canonically by itself, so no two paths share a name" $ do
        path <- forAll genPath
        viaNonEmpty head (namesOf path) === Just path
