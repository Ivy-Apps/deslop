module Frontend.Nix.ModuleNameSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deslop.Module (ModuleName (..))
import FileSystem.Path (portablePath)
import Frontend.Nix.ModuleName (externalName, moduleNames, projectNames)
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop, rp)

namesOf :: Text -> [Text]
namesOf = fmap (.text) . toList . moduleNames . rp

-- | Every path's names, given these paths are every module of the project.
namesAmong :: [Text] -> Map Text [Text]
namesAmong = Map.fromList . fmap (bimap portablePath (fmap (.text) . toList)) . Map.toList . projectNames . fmap rp

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
    it "names a Nix file by its path without the extension" $
        namesOf "modules/home/git.nix" `shouldBe` ["modules/home/git"]

    it "names a default.nix by its path, aliased by its directory" $
        namesOf "lib/default.nix" `shouldBe` ["lib/default", "lib"]

    it "names the root's default.nix default, with no alias" $
        namesOf "default.nix" `shouldBe` ["default"]

    it "names flake.nix flake" $
        namesOf "flake.nix" `shouldBe` ["flake"]

    it "keeps the extension of what is not Nix" $
        namesOf "secrets/common.yaml" `shouldBe` ["secrets/common.yaml"]

    it "names a directory by its path" $
        namesOf "programs/agents/skills" `shouldBe` ["programs/agents/skills"]

    it "names a module outside the project by its segments, without the extension" $
        fmap (.text) [externalName ("nixpkgs" :| []), externalName ("nixpkgs" :| ["lib", "default.nix"])]
            `shouldBe` ["nixpkgs", "nixpkgs/lib/default"]

    describe "among every module of the project" $ do
        it "names what is not Nix ./<path> beside the .nix file of its name, aliased by its path" $
            namesAmong ["pkg", "pkg.nix", "deploy", "deploy.nix", "common.yaml", "common.yaml.nix"]
                `shouldBe` Map.fromList
                    [ ("pkg", ["./pkg", "pkg"])
                    , ("pkg.nix", ["pkg"])
                    , ("deploy", ["./deploy", "deploy"])
                    , ("deploy.nix", ["deploy"])
                    , ("common.yaml", ["./common.yaml", "common.yaml"])
                    , ("common.yaml.nix", ["common.yaml"])
                    ]

        it "names what is not Nix by its path when no .nix file is called that" $
            namesAmong ["pkg", "lib/default.nix"] `shouldBe` Map.fromList [("pkg", ["pkg"]), ("lib/default.nix", ["lib/default", "lib"])]

        prop "gives every module a canonical name no other module has" $ do
            paths <- forAll (ordNub <$> Gen.list (Range.linear 0 30) genPath)
            let canonical = mapMaybe (viaNonEmpty head) (Map.elems (namesAmong paths))
            length (ordNub canonical) === length canonical

        prop "keeps a .nix file's names, and every name a module has by its path alone" $ do
            paths <- forAll (ordNub <$> Gen.list (Range.linear 0 30) genPath)
            for_ (Map.toList (namesAmong paths)) $ \(path, names) ->
                assert $
                    if ".nix" `T.isSuffixOf` path
                        then names == namesOf path
                        else names `elem` [namesOf path, ("./" <> path) : namesOf path]
