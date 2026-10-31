module Frontend.Nix.ModuleSpec (spec) where

import Data.Text qualified as T
import Deslop.Module (DependencyEdge (..), EdgeTarget (..), Location (..), Module (..), ModuleName (..), ModuleRole (Source), canonicalName)
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
import System.Directory (createDirectoryIfMissing, createDirectoryLink, createFileLink)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import TestUtils (prop, requireRight)
import UnliftIO.Temporary (withSystemTempDirectory)

-- | A real project with these files, and a symbolic link, by how it is made, its target first, beside them.
loweredWithLink :: (FilePath -> FilePath -> IO ()) -> (FilePath, FilePath) -> [(FilePath, ByteString)] -> IO (Lowered NixFrontend)
loweredWithLink makeLink (target, link) files = withSystemTempDirectory "deslop-nix-module-links" $ \tmp -> do
    traverse_ (\(path, content) -> createDirectoryIfMissing True (takeDirectory (tmp </> path)) >> writeFileBS (tmp </> path) content) files
    makeLink (tmp </> target) (tmp </> link)
    let root = ProjectRoot (absPathUnsafe (encodeOsPathString tmp))
    requireRight (toString . Renderable.render) <=< runEff . runConcurrent . runFileSystemIO . runReader root $ lowerProjectAt NixFrontend root

-- | These files, by their path from the project root, through every stage; the test fails when the project does not load.
lowered :: [(Text, ByteString)] -> IO (Lowered NixFrontend)
lowered = requireRight (toString . Renderable.render) <=< lowerProject NixFrontend

canonicalNames :: Lowered NixFrontend -> [Text]
canonicalNames = sort . fmap ((.text) . canonicalName) . (.modules)

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
        it "lowers every file of the project, and every directory a path reaches" $
            canonicalNames result
                `shouldBe` sort
                    [ "broken.nix"
                    , "deslop/nix.yaml"
                    , "flake.nix"
                    , "hosts/one/configuration.nix"
                    , "hosts/two/configuration.nix"
                    , "lib/default.nix"
                    , "modules/a.nix"
                    , "modules/b.nix"
                    , "modules/skills/x"
                    , "modules/skills/x/SKILL.md"
                    , "secrets/common.yaml"
                    ]

        it "aliases a default.nix by its directory" $
            fmap (fmap (.text) . toList . (.names)) (moduleNamed "lib/default.nix" result) `shouldBe` Just ["lib/default.nix", "lib"]

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
            edgeTargetsFrom "flake.nix" result `shouldBe` [("./lib", "lib/default.nix")]

        it "reads a path from the root, and expands a computed one to every match" $
            edgeTargetsFrom "lib/default.nix" result
                `shouldBe` [ ("\"${root}/modules/a.nix\"", "modules/a.nix")
                           , ("\"${root}/hosts/${hostname}/configuration.nix\"", "hosts/one/configuration.nix")
                           , ("\"${root}/hosts/${hostname}/configuration.nix\"", "hosts/two/configuration.nix")
                           , ("inputs.nixpkgs", "External:inputs/nixpkgs")
                           ]

        it "reads a cycle, a missing file, an asset, a pattern and a computed path" $
            edgeTargetsFrom "modules/a.nix" result
                `shouldBe` [ ("./b.nix", "modules/b.nix")
                           , ("../gone.nix", "Unresolved:../gone.nix")
                           , ("../secrets/common.yaml", "secrets/common.yaml")
                           , ("./skills + \"/${name}\"", "modules/skills/x")
                           , ("builtins.readDir dir", "Unresolved:builtins.readDir dir")
                           ]

        it "names a search path entry and a NixOS module as externals" $
            (edgeTargetsFrom "modules/b.nix" result, edgeTargetsFrom "hosts/one/configuration.nix" result)
                `shouldBe` ( [("./a.nix", "modules/a.nix"), ("<nixpkgs>", "External:nixpkgs")]
                           , [("modulesPath + \"/profiles/qemu-guest.nix\"", "External:inputs/nixpkgs/nixos/modules/profiles/qemu-guest.nix")]
                           )

        it "quotes the line an edge is written on" $
            fmap (fmap (\e -> (e.location.line, e.location.code)) . take 1 . (.edges)) (moduleNamed "modules/b.nix" result)
                `shouldBe` Just [(1, "{ imports = [ ./a.nix (import <nixpkgs> { }) ]; }")]

        it "gives a module it cannot read no edges, and says so" $ do
            fmap (.edges) (moduleNamed "broken.nix" result) `shouldBe` Just []
            fmap Renderable.render result.warnings `shouldBe` ["broken.nix could not be read as Nix, so what it refers to was not read."]

    describe "a file nothing refers to" $ do
        whole <-
            runIO . lowered $
                [ ("flake.nix", "{ }")
                , ("docs/x.md", "# x\n")
                , ("a.py", "import b\n")
                , ("logo.bin", "\xff\xfe\x00")
                ]

        it "is a module with no edges, its file name its name" $
            [(portablePath m.path, (canonicalName m).text, length m.edges, m.role) | m <- whole.modules, portablePath m.path /= "flake.nix"]
                `shouldBe` [("a.py", "a.py", 0, Source), ("docs/x.md", "docs/x.md", 0, Source), ("logo.bin", "logo.bin", 0, Source)]

        it "says nothing about it" $
            fmap Renderable.render whole.warnings `shouldBe` []

    describe "a gitignored file" $ do
        ignoring <-
            runIO . lowered $
                [ ("flake.nix", "{ a = ./reached.md; }")
                , (".gitignore", "*.md\n")
                , ("reached.md", "")
                , ("unreached.md", "")
                ]

        it "is a module only when a path reaches it" $
            canonicalNames ignoring `shouldBe` [".gitignore", "flake.nix", "reached.md"]

    describe "a directory a path reaches" $ do
        beside <-
            runIO . lowered $
                [ ("flake.nix", "{ }")
                , ("pkg.nix", "{ src = ./pkg; script = ./deploy; }")
                , ("pkg/x.txt", "")
                , ("deploy", "")
                , ("deploy.nix", "{ }")
                ]

        it "is a module, beside the files in it and with no edges to them" $ do
            canonicalNames beside
                `shouldBe` ["deploy", "deploy.nix", "flake.nix", "pkg", "pkg.nix", "pkg/x.txt"]
            fmap (.edges) (moduleNamed "pkg" beside) `shouldBe` Just []

        it "shares no name with the .nix file of its stem, so nothing warns" $ do
            edgeTargetsFrom "pkg.nix" beside `shouldBe` [("./pkg", "pkg"), ("./deploy", "deploy")]
            fmap Renderable.render beside.warnings `shouldBe` []

    it "tells foo.nix from foo/default.nix" $ do
        besideDirectory <- lowered [("default.nix", "./foo"), ("foo.nix", "{ }"), ("foo/default.nix", "{ }")]

        edgeTargetsFrom "default.nix" besideDirectory `shouldBe` [("./foo", "foo/default.nix")]
        fmap Renderable.render besideDirectory.warnings `shouldBe` []

    describe "a symbolic link" $ do
        linked <-
            runIO . loweredWithLink createFileLink ("real.nix", "zlink.nix") $
                [("flake.nix", "{ }"), ("default.nix", "{ a = import ./real.nix; }"), ("real.nix", "{ }")]

        it "to a module names each by its own path" $
            canonicalNames linked `shouldBe` ["default.nix", "flake.nix", "real.nix", "zlink.nix"]

        it "to a module names the edge by the path written" $
            edgeTargetsFrom "default.nix" linked `shouldBe` [("./real.nix", "real.nix")]

        it "to a file that is not Nix is a module of its own, as the walk found it" $ do
            notes <- loweredWithLink createFileLink ("real.md", "zlink.md") [("flake.nix", "{ }"), ("real.md", "")]

            canonicalNames notes `shouldBe` ["flake.nix", "real.md", "zlink.md"]

        it "to a directory leaves the module, and every edge through the link, at the real path" $ do
            through <- loweredWithLink createDirectoryLink ("real", "link") [("flake.nix", "{ a = ./link/x.md; }"), ("real/x.md", "")]

            edgeTargetsFrom "flake.nix" through `shouldBe` [("./link/x.md", "real/x.md")]
            canonicalNames through `shouldBe` ["flake.nix", "real/x.md"]

    prop "reaches exactly the files a project's literal paths name" $ do
        targets <- forAll $ Gen.subsequence ["a", "b", "c", "d"]
        let files = ("default.nix", encodeUtf8 ("[ " <> T.unwords ["./" <> t <> ".nix" | t <- targets] <> " ]")) : [(t <> ".nix", "{ }") | t <- ["a", "b", "c", "d"]]
        result' <- liftIO (lowered files)
        fmap snd (edgeTargetsFrom "default.nix" result') === fmap (<> ".nix") targets

    it "keeps every module's path from the project root" $
        fmap (portablePath . (.path)) (moduleNamed "modules/a.nix" result) `shouldBe` Just "modules/a.nix"
