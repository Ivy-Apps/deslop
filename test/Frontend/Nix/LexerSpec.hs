module Frontend.Nix.LexerSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful (runEff)
import Effects.FileSystem (runRoFileSystemIO)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), portablePath, relativePathTo)
import Fixtures.OnDisk (Language (..), projectIn)
import Frontend.Nix.Ignore (nixEcosystemIgnored)
import Frontend.Nix.Iterator (getNixFiles)
import Frontend.Nix.Lexeme (Lexeme (..), Part (..), Quoting (..), isTrivia)
import Frontend.Nix.Lexer (PathLiteralSpan (..), lexNix, pathLiteralSpans)
import Git.Ignore (loadGitIgnore)
import Renderable qualified
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import TestUtils (mkAbsolute, requireJust)

-- | The lexemes of the source, without whitespace and comments.
lexed :: Text -> Maybe [Lexeme]
lexed = fmap (filter (not . isTrivia)) . lexNix

-- | Just the path literals, as written.
pathsIn :: Text -> Maybe [Text]
pathsIn = fmap (fmap (.literal)) . pathLiteralSpans

{- | Every @.nix@ file of the NixOS fixture, as the Nix frontend discovers it,
and the fixture's root.
-}
nixosFiles :: IO (ProjectRoot, [AbsPath])
nixosFiles = do
    root <- ProjectRoot <$> mkAbsolute nixosFixture
    files <- runEff . runRoFileSystemIO $ do
        gitIgnore <- loadGitIgnore nixEcosystemIgnored root.path
        getNixFiles gitIgnore root
    pure (root, files)

nixosFixture :: OsPath
nixosFixture = projectIn Nix "nix-nixos-config"

spec :: Spec
spec = describe "Frontend.Nix.Lexer" $ do
    describe "paths" $ do
        it "reads a relative, a parent, an absolute and a home path" $
            pathsIn "[ ./a.nix ../b/c.nix /etc/hosts ~/x ]" `shouldBe` Just ["./a.nix", "../b/c.nix", "/etc/hosts", "~/x"]

        it "reads the own directory as a path" $
            pathsIn "src = ./.;" `shouldBe` Just ["./."]

        it "reads a path with no dot, as Nix does" $
            pathsIn "x = a/b;" `shouldBe` Just ["a/b"]

        it "keeps an interpolated path whole" $
            lexed "./plugins/${name}.nix" `shouldBe` Just [PathLit [Chunk "./plugins/", Interpolation [Identifier "name"], Chunk ".nix"]]

        it "rejects a path ending in a slash, as Nix does" $
            lexed "./a/" `shouldBe` Nothing

        it "rejects a path whose slash opens a comment or an update" $
            (lexed "./a/*c*/", lexed "a: b: c: a/b//c") `shouldBe` (Nothing, Nothing)

        it "rejects a path ending in a slash after an interpolation" $
            lexed "n: ./a/${n}/" `shouldBe` Nothing

        it "rejects a path ending in a slash inside an interpolation" $
            lexed "n: \"${./a/}\"" `shouldBe` Nothing

        it "reads a slash an interpolation follows as part of the path" $
            pathsIn "n: ./a/${n}" `shouldBe` Just ["./a/${n}"]

        it "reads a path and a slash apart when space parts them" $
            pathsIn "a: b: [ ./a / b ]" `shouldBe` Just ["./a"]

        it "reads division with spaces and the update operator as operators" $
            pathsIn "a / b // c" `shouldBe` Just []

        it "reads a search path as one lexeme" $
            lexed "import <nixpkgs/lib> { }" `shouldBe` Just [Identifier "import", SearchPath "nixpkgs/lib", Punctuation "{", Punctuation "}"]

        it "reads an unquoted URI as one lexeme, not a path" $
            lexed "github:NixOS/nixpkgs" `shouldBe` Just [Uri "github:NixOS/nixpkgs"]

        it "reads a URI whose scheme holds a plus, a minus and a dot as one lexeme" $
            lexed "git+ssh.a-b://host/repo" `shouldBe` Just [Uri "git+ssh.a-b://host/repo"]

    describe "comments" $ do
        it "finds no path in a line comment" $
            pathsIn "# import ./a.nix\nx" `shouldBe` Just []

        it "finds no path in a block comment, which does not nest" $
            pathsIn "/* ./a.nix /* */ ./b.nix" `shouldBe` Just ["./b.nix"]

        it "reads a block comment after a path character as a comment" $
            pathsIn "a/*./b.nix*/" `shouldBe` Just []

    describe "strings" $ do
        it "finds no path in a string's own text" $
            pathsIn "\"./a.nix\" ''./b.nix''" `shouldBe` Just []

        it "finds a path in a string's interpolation" $
            pathsIn "\"cp ${./a.sh} $out\"" `shouldBe` Just ["./a.sh"]

        it "finds a path in an indented string's interpolation, nested in braces" $
            pathsIn "''\n  ${builtins.readFile ./ctl/paths.sh}\n  ${ { a = ./b.nix; }.a }\n''" `shouldBe` Just ["./ctl/paths.sh", "./b.nix"]

        it "does not end a double-quoted string at an escaped quote" $
            lexed "\"a\\\"./b\" ./c" `shouldBe` Just [Str DoubleQuoted [Chunk "a\\\"./b"], PathLit [Chunk "./c"]]

        it "reads $${ as text in a double-quoted string" $
            lexed "\"$${x}\"" `shouldBe` Just [Str DoubleQuoted [Chunk "$${x}"]]

        it "ends an interpolation at its own brace, not at one the code opened" $
            lexed "\"${{a}.a}b\""
                `shouldBe` Just
                    [ Str
                        DoubleQuoted
                        [ Interpolation [Punctuation "{", Identifier "a", Punctuation "}", Punctuation ".", Identifier "a"]
                        , Chunk "b"
                        ]
                    ]

        it "reads the escapes of an indented string as its text" $
            lexed "'' ''' ''${x} ''\\n ''" `shouldBe` Just [Str Indented [Chunk " ''' ''${x} ''\\n "]]

        it "interpolates after a lone quote in an indented string" $
            lexed "'''${x}''" `shouldBe` Just [Str Indented [Chunk "'", Interpolation [Identifier "x"]]]

        it "ends an indented string at two quotes" $
            lexed "'' a '' b" `shouldBe` Just [Str Indented [Chunk " a "], Identifier "b"]

    describe "what Nix would not read either" $ do
        it "refuses an unterminated string" $
            lexNix "x = \"abc" `shouldBe` Nothing

        it "refuses an unterminated block comment" $
            lexNix "/* x" `shouldBe` Nothing

        it "refuses an interpolation never closed" $
            lexNix "\"${x\"" `shouldBe` Nothing

    describe "spans" $ do
        it "counts in bytes of UTF-8" $
            pathLiteralSpans "\"é\" ./a" `shouldBe` Just [PathLiteralSpan {start = 5, end = 8, literal = "./a"}]

        it "spans a path inside an interpolation where it is written" $ do
            let source = "''${./x.sh}''"
            spans <- requireJust "lexes" (pathLiteralSpans source)

            fmap (\s -> BS.take (s.end - s.start) (BS.drop s.start (encodeUtf8 source))) spans `shouldBe` ["./x.sh"]

    describe "the NixOS fixture" $ do
        (root, files) <- runIO nixosFiles

        it "lexes every file and renders each back to its text" $ do
            files `shouldNotBe` []
            for_ files $ \file -> do
                text <- decodeUtf8 <$> SFO.readFile' file.osPath
                let name = portablePath (relativePathTo root file)
                (name, Renderable.render <$> lexNix text) `shouldBe` (name, Just (text :: Text))

        it "finds the path literals a reader finds" $ do
            text <- decodeUtf8 <$> SFO.readFile' (nixosFixture </> [osp|modules/home/factory/default.nix|])

            fmap (filter ("." `T.isPrefixOf`)) (pathsIn text)
                `shouldBe` Just ["../nvim.nix", "../typos.nix", "../languages/typescript.nix", "../languages/haskell.nix", "../languages/nix.nix", "./git.nix", "./ssh.nix", "./claude-factory.nix"]
