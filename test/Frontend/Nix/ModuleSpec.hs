module Frontend.Nix.ModuleSpec (spec) where

import Data.Text qualified as T
import Deslop.Module (DependencyEdge (..), EdgeTarget (..), Location (..), Module (..), ModuleName (..), canonicalName)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (runFileSystemIO)
import FileSystem.Path (ProjectRoot (..), absPathUnsafe, encodeOsPathString, portablePath)
import Fixtures.Deslop.Frontend (Lowered (..), edgeTargetsFrom, lowerProject, lowerProjectAt, moduleNamed)
import Frontend.Nix (NixFrontend (..))
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Renderable qualified
import System.Directory (createFileLink)
import System.FilePath ((</>))
import Test.Hspec
import TestUtils (prop, requireRight)
import UnliftIO.Temporary (withSystemTempDirectory)

{- | A real project with these files, and @zlink.nix@ a symbolic link to
@real.nix@ beside them.
-}
loweredWithLink :: [(FilePath, ByteString)] -> IO (Lowered NixFrontend)
loweredWithLink files = withSystemTempDirectory "deslop-nix-module-links" $ \tmp -> do
    traverse_ (\(path, content) -> writeFileBS (tmp </> path) content) files
    createFileLink (tmp </> "real.nix") (tmp </> "zlink.nix")
    let root = ProjectRoot (absPathUnsafe (encodeOsPathString tmp))
    requireRight (toString . Renderable.render) <=< runEff . runConcurrent . runFileSystemIO . runReader root $ lowerProjectAt NixFrontend root

-- | These files, by their path from the project root, through every stage; the test fails when the project does not load.
lowered :: [(Text, ByteString)] -> IO (Lowered NixFrontend)
lowered = requireRight (toString . Renderable.render) <=< lowerProject NixFrontend

project :: [(Text, ByteString)]
project =
    [ ("flake.nix", "{\n  inputs.nixpkgs.url = \"github:NixOS/nixpkgs\";\n  outputs = { self, ... }@inputs: {\n    lib = import ./lib { inherit inputs; root = self; };\n  };\n}\n")
    , ("deslop/nix.yaml", "roots: [root]\n")
    , ("lib/default.nix", "{ inputs, root }: {\n  modules = [ \"${root}/modules/a.nix\" \"${root}/hosts/${hostname}/configuration.nix\" ];\n  pkgs = import inputs.nixpkgs { };\n  self = ./.;\n  again = ./default.nix;\n}\n")
    , ("modules/a.nix", "{ ... }: {\n  imports = [ ./b.nix ./b.nix ../gone.nix ];\n  secret = ../secrets/common.yaml;\n  skills = ./skills + \"/${name}\";\n  listed = builtins.readDir dir;\n}\n")
    , ("modules/b.nix", "{ imports = [ ./a.nix (import <nixpkgs> { }) ]; }\n")
    , ("modules/skills/x/SKILL.md", "")
    , ("hosts/one/configuration.nix", "{ modulesPath, ... }: { imports = [ (modulesPath + \"/profiles/qemu-guest.nix\") ]; }\n")
    , ("hosts/two/configuration.nix", "{ }\n")
    , ("secrets/common.yaml", "")
    , ("broken.nix", "\"never closed")
    ]

spec :: Spec
spec = describe "Frontend.Nix.Module" $ do
    result <- runIO (lowered project)

    describe "naming" $ do
        it "lowers every .nix file, and every other file a path reaches" $
            sort (fmap ((.text) . canonicalName) result.modules)
                `shouldBe` sort
                    [ "broken"
                    , "flake"
                    , "hosts/one/configuration"
                    , "hosts/two/configuration"
                    , "lib/default"
                    , "modules/a"
                    , "modules/b"
                    , "modules/skills/x"
                    , "secrets/common.yaml"
                    ]

        it "aliases a default.nix by its directory" $
            fmap (fmap (.text) . toList . (.names)) (moduleNamed "lib/default" result) `shouldBe` Just ["lib/default", "lib"]

        it "names every edge into a module by the module's own names" $
            [ (names, target.names)
            | m <- result.modules
            , Resolved targetId names <- fmap (.target) m.edges
            , target <- result.modules
            , target.id == targetId
            ]
                `shouldSatisfy` all (uncurry (==))

    describe "edges" $ do
        it "follows a path to a directory to its default.nix" $
            edgeTargetsFrom "flake" result `shouldBe` [("./lib", "lib/default")]

        it "reads a path from the root, and expands a computed one to every match" $
            edgeTargetsFrom "lib/default" result
                `shouldBe` [ ("\"${root}/modules/a.nix\"", "modules/a")
                           , ("\"${root}/hosts/${hostname}/configuration.nix\"", "hosts/one/configuration")
                           , ("\"${root}/hosts/${hostname}/configuration.nix\"", "hosts/two/configuration")
                           , ("inputs.nixpkgs", "External:inputs/nixpkgs")
                           ]

        it "reads a cycle, a missing file, an asset, a pattern and a computed path" $
            edgeTargetsFrom "modules/a" result
                `shouldBe` [ ("./b.nix", "modules/b")
                           , ("../gone.nix", "Unresolved:../gone.nix")
                           , ("../secrets/common.yaml", "secrets/common.yaml")
                           , ("./skills + \"/${name}\"", "modules/skills/x")
                           , ("builtins.readDir dir", "Unresolved:builtins.readDir dir")
                           ]

        it "names a search path entry and a NixOS module as externals" $
            (edgeTargetsFrom "modules/b" result, edgeTargetsFrom "hosts/one/configuration" result)
                `shouldBe` ( [("./a.nix", "modules/a"), ("<nixpkgs>", "External:nixpkgs")]
                           , [("modulesPath + \"/profiles/qemu-guest.nix\"", "External:inputs/nixpkgs/nixos/modules/profiles/qemu-guest")]
                           )

        it "quotes the line an edge is written on" $
            fmap (fmap (\e -> (e.location.line, e.location.code)) . take 1 . (.edges)) (moduleNamed "modules/b" result)
                `shouldBe` Just [(1, "{ imports = [ ./a.nix (import <nixpkgs> { }) ]; }")]

        it "gives a module it cannot read no edges, and says so" $ do
            fmap (.edges) (moduleNamed "broken" result) `shouldBe` Just []
            fmap Renderable.render result.warnings `shouldBe` ["broken.nix could not be read as Nix, so what it refers to was not read."]

    describe "names two modules share" $ do
        it "warns of foo.nix beside foo/default.nix" $ do
            clash <- lowered [("default.nix", "./foo"), ("foo.nix", "{ }"), ("foo/default.nix", "{ }")]

            fmap Renderable.render clash.warnings `shouldBe` ["foo.nix, foo/default.nix share the name foo, so a Rule cannot tell them apart; rename or move all but one."]

        besideNix <-
            runIO . lowered $
                [ ("flake.nix", "{ }")
                , ("pkg.nix", "{ src = ./pkg; script = ./deploy; }")
                , ("pkg/x.txt", "")
                , ("deploy", "")
                , ("deploy.nix", "{ }")
                ]

        it "names what is not Nix ./<path> beside the .nix file of its name, aliased by its path" $
            sort (fmap (fmap (.text) . toList . (.names)) besideNix.modules)
                `shouldBe` [["./deploy", "deploy"], ["./pkg", "pkg"], ["deploy"], ["flake"], ["pkg"]]

        it "names every edge into what is not Nix by that name" $
            edgeTargetsFrom "pkg" besideNix `shouldBe` [("./pkg", "./pkg"), ("./deploy", "./deploy")]

        it "warns of what is not Nix beside the .nix file of its name" $
            fmap Renderable.render besideNix.warnings
                `shouldBe` [ "deploy, deploy.nix share the name deploy, so a Rule cannot tell them apart; rename or move all but one."
                           , "pkg, pkg.nix share the name pkg, so a Rule cannot tell them apart; rename or move all but one."
                           ]

    describe "a symbolic link to a module" $ do
        linked <-
            runIO . loweredWithLink $
                [("flake.nix", "{ }"), ("default.nix", "{ a = import ./real.nix; }"), ("real.nix", "{ }")]

        it "names each module by its own path" $
            sort (fmap ((.text) . canonicalName) linked.modules) `shouldBe` ["default", "flake", "real", "zlink"]

        it "names the edge by the path written" $
            edgeTargetsFrom "default" linked `shouldBe` [("./real.nix", "real")]

    prop "reaches exactly the files a project's literal paths name" $ do
        targets <- forAll $ Gen.subsequence ["a", "b", "c", "d"]
        let files = ("default.nix", encodeUtf8 ("[ " <> T.unwords ["./" <> t <> ".nix" | t <- targets] <> " ]")) : [(t <> ".nix", "{ }") | t <- ["a", "b", "c", "d"]]
        result' <- liftIO (lowered files)
        fmap snd (edgeTargetsFrom "default" result') === targets

    it "keeps every module's path from the project root" $
        fmap (portablePath . (.path)) (moduleNamed "modules/a" result) `shouldBe` Just "modules/a.nix"
