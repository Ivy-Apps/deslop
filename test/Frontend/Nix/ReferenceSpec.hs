module Frontend.Nix.ReferenceSpec (spec) where

import Data.Set qualified as Set
import Fixtures.Frontend.Nix.Reference (lit)
import Frontend.Nix.Lexer (lexNix)
import Frontend.Nix.Reference (Anchor (..), Piece (..), Reference (..), Referent (..), Scope (..), referencesIn)
import Test.Hspec

scope :: Scope
scope = Scope {roots = Set.fromList ["self", "root"], inputs = Set.fromList ["nixpkgs", "home-manager"]}

-- | What the source refers to, as written and as read.
referencesOf :: Text -> [(Text, Referent)]
referencesOf = maybe [] (fmap (\r -> (r.written, r.referent)) . referencesIn scope) . lexNix

spec :: Spec
spec = describe "Frontend.Nix.Reference" $ do
    describe "path literals" $ do
        it "reads every path literal, whatever it is used for" $
            referencesOf "{ imports = [ ./a.nix ]; src = ../b; x = builtins.readFile ./s.sh; }"
                `shouldBe` [ ("./a.nix", LocalPath FromFile [lit ".", lit "a.nix"])
                           , ("../b", LocalPath FromFile [lit "..", lit "b"])
                           , ("./s.sh", LocalPath FromFile [lit ".", lit "s.sh"])
                           ]

        it "skips the file's own directory" $
            referencesOf "{ src = ./.; }" `shouldBe` []

        it "reads an interpolated path as a pattern" $
            referencesOf "import ./plugins/${name}.nix"
                `shouldBe` [("./plugins/${name}.nix", LocalPath FromFile [lit ".", lit "plugins", [Hole, Literal ".nix"]])]

        it "reads a path with a string appended as one pattern" $
            referencesOf "./admin/skills + \"/${name}\""
                `shouldBe` [("./admin/skills + \"/${name}\"", LocalPath FromFile [lit ".", lit "admin", lit "skills", [Hole]])]

        it "reads an absolute or home path as one outside the project" $
            referencesOf "[ /etc/hosts ~/x ]"
                `shouldBe` [("/etc/hosts", LocalPath Elsewhere [lit "etc", lit "hosts"]), ("~/x", LocalPath Elsewhere [lit "~", lit "x"])]

        it "reads a path with a name appended as computed" $
            referencesOf "./. + name" `shouldBe` [("./. + name", Computed)]

        it "finds a path inside an interpolation" $
            referencesOf "''\n  ${builtins.readFile ./ctl/paths.sh}\n''"
                `shouldBe` [("./ctl/paths.sh", LocalPath FromFile [lit ".", lit "ctl", lit "paths.sh"])]

        it "finds nothing in comments and in a string's own text" $
            referencesOf "# ./a.nix\n/* ./b.nix */ \"./c.nix\"" `shouldBe` []

    describe "paths from the project root" $ do
        it "reads a string headed by a root" $
            referencesOf "\"${root}/modules/home\""
                `shouldBe` [("\"${root}/modules/home\"", LocalPath FromRoot [lit "modules", lit "home"])]

        it "reads a root followed by a computed tail as a pattern" $
            referencesOf "\"${root}/hosts/${hostname}/configuration.nix\""
                `shouldBe` [("\"${root}/hosts/${hostname}/configuration.nix\"", LocalPath FromRoot [lit "hosts", [Hole], lit "configuration.nix"])]

        it "reads self as a root" $
            referencesOf "import \"${self}/lib\"" `shouldBe` [("\"${self}/lib\"", LocalPath FromRoot [lit "lib"])]

        it "reads a root with a string appended" $
            referencesOf "root + \"/x.nix\"" `shouldBe` [("root + \"/x.nix\"", LocalPath FromRoot [lit "x.nix"])]

        it "skips the root itself" $
            referencesOf "{ src = \"${self}\"; }" `shouldBe` []

        it "reads a string headed by a name that is not a root as no path" $
            referencesOf "\"${config.home.homeDirectory}/.config\"" `shouldBe` []

        it "reads an attribute named like a root as no root" $
            referencesOf "x.root + \"/a\"" `shouldBe` [("x.root + \"/a\"", Computed)]

    describe "flake inputs" $ do
        it "reads a declared input up to its first computed attribute" $
            referencesOf "inputs.home-manager.packages.${system}.default"
                `shouldBe` [("inputs.home-manager.packages", FlakeInput "home-manager" ["packages"])]

        it "reads an input with no attribute" $
            referencesOf "import inputs.nixpkgs { }" `shouldBe` [("inputs.nixpkgs", FlakeInput "nixpkgs" [])]

        it "reads an input that is not declared as nothing" $
            referencesOf "inputs.other.lib" `shouldBe` []

        it "reads a follows binding as nothing" $
            referencesOf "{ inputs.nixpkgs.follows = \"nixpkgs\"; }" `shouldBe` []

        it "reads an inputs attribute of something else as nothing" $
            referencesOf "x.inputs.nixpkgs" `shouldBe` []

    describe "what nixpkgs holds" $ do
        it "reads modulesPath with a string appended as a NixOS module" $
            referencesOf "(modulesPath + \"/profiles/qemu-guest.nix\")"
                `shouldBe` [("modulesPath + \"/profiles/qemu-guest.nix\"", NixosModule [lit "profiles", lit "qemu-guest.nix"])]

        it "reads a search path by its segments" $
            referencesOf "import <nixpkgs/lib> { }" `shouldBe` [("<nixpkgs/lib>", SearchPathEntry ["nixpkgs", "lib"])]

    describe "paths only evaluation names" $ do
        it "reads a name with a path-shaped string appended as computed" $
            referencesOf "dir + \"/${name}\"" `shouldBe` [("dir + \"/${name}\"", Computed)]

        it "reads a selection with a path-shaped string appended as computed" $
            referencesOf "source.src + \"/${root}\"" `shouldBe` [("source.src + \"/${root}\"", Computed)]

        it "reads a directory listing of a name as computed" $
            referencesOf "builtins.readDir dir" `shouldBe` [("builtins.readDir dir", Computed)]

        it "reads an import of a name as computed" $
            referencesOf "import pkgsPath { }" `shouldBe` [("import pkgsPath", Computed)]

        it "reads a string appended to a name as no path when it does not start with a slash" $
            referencesOf "name + \".nix\"" `shouldBe` []

        it "reads a root followed by text that is no path as computed" $
            referencesOf "\"${root}x\"" `shouldBe` [("\"${root}x\"", Computed)]

    it "gives the offset each reference is written at" $
        maybe [] (fmap (.offset) . referencesIn scope) (lexNix "[ ./a\n  ./b ]") `shouldBe` [2, 8]
