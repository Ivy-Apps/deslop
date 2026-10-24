module Frontend.Nix.FlakeInputsSpec (spec) where

import Data.Set qualified as Set
import Fixtures.OnDisk (Language (..), projectIn)
import Frontend.Nix.FlakeInputs (declaredInputs)
import Frontend.Nix.Lexer (lexNix)
import System.File.OsPath qualified as SFO
import System.OsPath (osp, (</>))
import Test.Hspec

inputsOf :: Text -> [Text]
inputsOf = maybe [] (Set.toList . declaredInputs) . lexNix

spec :: Spec
spec = describe "Frontend.Nix.FlakeInputs" $ do
    it "reads every attribute of the inputs set, however it is spelled" $
        inputsOf
            "{\n\
            \  inputs = {\n\
            \    nixpkgs.url = \"github:NixOS/nixpkgs\";\n\
            \    home-manager = { url = \"github:nix-community/home-manager\"; inputs.nixpkgs.follows = \"nixpkgs\"; };\n\
            \    \"quoted\".url = \"path:./q\";\n\
            \  };\n\
            \  outputs = { self, ... }: { };\n\
            \}\n"
            `shouldBe` ["home-manager", "nixpkgs", "quoted"]

    it "reads inputs bound one by one beside the outputs" $
        inputsOf "{ inputs.nixpkgs.url = \"github:NixOS/nixpkgs\"; inputs.disko = { url = \"github:nix-community/disko\"; }; outputs = _: { }; }"
            `shouldBe` ["disko", "nixpkgs"]

    it "does not take an input's own inputs for the flake's" $
        inputsOf "{ inputs = { a = { inputs.b.follows = \"c\"; }; }; }" `shouldBe` ["a"]

    it "reads no inputs from a flake without any" $
        inputsOf "{ outputs = { self }: { inputs = { x = 1; }; }; }" `shouldBe` []

    it "reads the inputs of the NixOS fixture" $ do
        text <- decodeUtf8 <$> SFO.readFile' (projectIn Nix "nix-nixos-config" </> [osp|flake.nix|])

        inputsOf text
            `shouldBe` ["disko", "herdr", "home-manager", "lanzaboote", "llm-agents", "mattpocock-skills", "nix-darwin", "nixpkgs", "nixvim", "sops-nix"]
