module Frontend.Haskell.CabalSpec (spec) where

import Data.Text qualified as T
import Deslop.Error (Warning (..))
import Doubles.FileSystem (newInMemoryFileSystem, runInMemoryFileSystem)
import Effectful (runEff)
import FileSystem.Path (AbsPath (..), decodeOsPath)
import Fixtures.Frontend.Haskell.Project (inProject, projectRoot)
import Frontend.Haskell.Cabal (
    CabalError (..),
    Component (..),
    ComponentKind (..),
    HsConfig (..),
    MainIs (..),
    OwnedAs (..),
    Owner (..),
    loadHsConfig,
    ownerOf,
    qualifier,
 )
import Frontend.Haskell.ModuleName (dotted)
import Frontend.Haskell.Preprocessor (Preprocessor (..), Request (..), RequestedBy (..))
import System.OsPath (osp)
import Test.Hspec
import TestUtils (requireRight)

loaded :: [(Text, Text)] -> IO (Either CabalError (HsConfig, [Warning]))
loaded files = do
    fs <- newInMemoryFileSystem [(inProject path, encodeUtf8 text) | (path, text) <- files]
    runEff . runInMemoryFileSystem fs $ loadHsConfig projectRoot

config :: Text -> IO HsConfig
config cabal = fmap fst . requireRight show =<< loaded [("p.cabal", cabal)]

package :: [Text] -> Text
package stanzas = T.unlines (["cabal-version: 3.0", "name: p", "version: 0", ""] <> stanzas)

-- | Each component as its qualifier, source directories and main-is.
shapes :: HsConfig -> [(Text, [Text], Maybe MainIs)]
shapes cfg = [(qualifier c, relative <$> toList c.sourceDirs, mainOf c.kind) | c <- cfg.components]
  where
    relative dir =
        let path = decodeOsPath dir.osPath
         in fromMaybe path (T.stripPrefix "/project/" path <|> ("." <$ guard (path == "/project")))
    mainOf = \case
        Library -> Nothing
        Executable m -> Just m
        TestSuite m -> m
        Benchmark m -> m

componentNamed :: Text -> HsConfig -> Maybe Component
componentNamed q = find ((== q) . qualifier) . (.components)

spec :: Spec
spec = describe "Frontend.Haskell.Cabal" $ do
    describe "components" $ do
        it "are read in cabal's order, each qualified by its kind and name" $ do
            cfg <-
                config . package $
                    [ "benchmark p-bench\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n  hs-source-dirs: bench"
                    , "test-suite p-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs: test"
                    , "executable p\n  main-is: Main.hs\n  hs-source-dirs: app"
                    , "library\n  hs-source-dirs: src"
                    , "library inner\n  hs-source-dirs: inner"
                    ]

            shapes cfg
                `shouldBe` [ ("lib/p", ["src"], Nothing)
                           , ("lib/inner", ["inner"], Nothing)
                           , ("exe/p", ["app"], Just (MainIs [osp|Main.hs|]))
                           , ("test/p-test", ["test"], Just (MainIs [osp|Spec.hs|]))
                           , ("bench/p-bench", ["bench"], Just (MainIs [osp|Bench.hs|]))
                           ]

        it "read from the package's directory when they name no source directory" $ do
            cfg <- config (package ["library"])

            shapes cfg `shouldBe` [("lib/p", ["."], Nothing)]

        it "normalise a source directory written with . and .." $ do
            cfg <- config (package ["library\n  hs-source-dirs: ./src/../lib/."])

            shapes cfg `shouldBe` [("lib/p", ["lib"], Nothing)]

        it "take the source directories of a common stanza they import" $ do
            cfg <- config (package ["common shared\n  hs-source-dirs: shared", "library\n  import: shared\n  hs-source-dirs: src"])

            shapes cfg `shouldBe` [("lib/p", ["shared", "src"], Nothing)]

        -- A Rule must hold in every build, not only the one this machine makes.
        it "take the source directories of every conditional branch" $ do
            cfg <-
                config . package $
                    ["flag fast\n  default: False", "library\n  if flag(fast)\n    hs-source-dirs: fast\n  else\n    hs-source-dirs: slow"]

            shapes cfg `shouldBe` [("lib/p", ["fast", "slow"], Nothing)]

        it "have no Main when their interface names none" $ do
            cfg <- config (package ["test-suite t\n  type: detailed-0.9\n  test-module: T\n  hs-source-dirs: test"])

            shapes cfg `shouldBe` [("test/t", ["test"], Nothing)]

    describe "the project libraries a component sees" $ do
        let cfgWith depends =
                config . package $
                    [ "library\n  hs-source-dirs: src\n  build-depends: base, inner"
                    , "library inner\n  hs-source-dirs: inner"
                    , "executable e\n  main-is: Main.hs\n  build-depends: " <> depends
                    ]
            seenBy q = fmap ((.libraries) <=< toList . componentNamed q)

        it "are none when it depends on other packages only" $
            seenBy "exe/e" (cfgWith "base, text") `shouldReturn` []

        it "are the main library when it depends on its own package" $
            seenBy "exe/e" (cfgWith "base, p >= 0") `shouldReturn` ["p"]

        it "are a sub-library named through the package" $
            seenBy "exe/e" (cfgWith "p:inner") `shouldReturn` ["inner"]

        it "are a sub-library named on its own, as older cabal files do" $
            seenBy "lib/p" (cfgWith "base") `shouldReturn` ["inner"]

        it "are several libraries named at once" $
            seenBy "exe/e" (cfgWith "p:{p, inner}") `shouldReturn` ["p", "inner"]

    describe "preprocessors" $ do
        it "are CPP when a component enables it by default" $ do
            cfg <- config (package ["library\n  default-extensions: OverloadedStrings, CPP"])

            fmap (.preprocessors) (componentNamed "lib/p" cfg) `shouldBe` Just [Request Cpp (ByComponent "lib:p")]

        -- other-extensions only declares what some file enables in a pragma.
        it "are nothing when a component lists CPP among its other extensions" $ do
            cfg <- config (package ["library\n  other-extensions: CPP"])

            fmap (.preprocessors) (componentNamed "lib/p" cfg) `shouldBe` Just []

        it "are a -pgmF program in a component's ghc-options" $ do
            cfg <- config (package ["executable e\n  main-is: Main.hs\n  ghc-options: -Wall -F -pgmF my-pp"])

            fmap (.preprocessors) (componentNamed "exe/e" cfg) `shouldBe` Just [Request (Custom "my-pp") (ByComponent "exe:e")]

    describe "the project" $ do
        it "is the package's name" $
            fmap (.package) (config (package [])) `shouldReturn` "p"

        it "accepts a cabal.project naming the root package" $ do
            result <- loaded [("p.cabal", package []), ("cabal.project", "packages: .\n")]

            fmap ((.package) . fst) result `shouldBe` Right "p"

        it "accepts a cabal.project naming the root .cabal file" $ do
            result <- loaded [("p.cabal", package []), ("cabal.project", "packages: ./p.cabal\nindex-state: 2026-01-01T00:00:00Z\n")]

            fmap ((.package) . fst) result `shouldBe` Right "p"

        it "accepts a cabal.project globbing only the root .cabal file" $ do
            result <- loaded [("p.cabal", package []), ("cabal.project", "packages: *.cabal ./*.cabal ./\n")]

            fmap ((.package) . fst) result `shouldBe` Right "p"

        it "accepts a cabal.project naming the root package by its absolute path" $ do
            result <- loaded [("p.cabal", package []), ("cabal.project", "packages: /project /project/p.cabal\n")]

            fmap ((.package) . fst) result `shouldBe` Right "p"

        it "refuses a cabal.project whose glob matches more than the root package" $ do
            result <- loaded [("p.cabal", package []), ("lib/lib.cabal", package []), ("cabal.project", "packages: . */*.cabal\n")]

            void result `shouldBe` Left (MultiPackageProject (inProject "cabal.project") ["*/*.cabal"])

        it "refuses a cabal.project naming other packages" $ do
            result <- loaded [("p.cabal", package []), ("cabal.project", "packages: .\n  other/\n  ./more\n")]

            void result `shouldBe` Left (MultiPackageProject (inProject "cabal.project") ["other/", "./more"])

        it "refuses a project with no .cabal file at its root" $ do
            result <- loaded [("sub/p.cabal", package [])]

            void result `shouldBe` Left (NotOneCabalFile [])

        it "refuses a project with two .cabal files at its root" $ do
            result <- loaded [("a.cabal", package []), ("b.cabal", package [])]

            void result `shouldBe` Left (NotOneCabalFile [inProject "a.cabal", inProject "b.cabal"])

        it "refuses a .cabal file Cabal cannot read, saying where" $ do
            result <- loaded [("p.cabal", "cabal-version: 3.0\nname: p\nversion: 0\nlibrary\n  build-depends: base >=\n")]

            case result of
                Left (CabalUnreadable path (e : _)) -> (path, "p.cabal:5" `T.isInfixOf` e) `shouldBe` (inProject "p.cabal", True)
                other -> expectationFailure ("expected the .cabal file to be unreadable, got " <> show other)

    describe "the component that owns a file" $ do
        let cfgOf =
                config . package $
                    [ "library\n  hs-source-dirs: ."
                    , "executable tool\n  main-is: Tool.hs\n  hs-source-dirs: ."
                    , "test-suite t\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs: test"
                    , "test-suite u\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test"
                    ]
            ownerFor path = fmap (\o -> (qualifier o.component, nameOf o.as)) . (`ownerOf` inProject path) <$> cfgOf
            nameOf = \case
                MainModule -> "Main"
                PathNamed n -> dotted n

        it "is the component whose main-is it is, whichever directories overlap" $
            ownerFor "Tool.hs" `shouldReturn` Just ("exe/tool", "Main")

        it "is the component whose deepest source directory holds it" $
            ownerFor "test/Deslop/ModuleSpec.hs" `shouldReturn` Just ("test/t", "Deslop.ModuleSpec")

        it "is the first component in the .cabal file when two share the directory" $
            ownerFor "src/Data/Thing.hs" `shouldReturn` Just ("lib/p", "src.Data.Thing")

        it "is named by its path below that directory" $
            ownerFor "test/Helpers.hs" `shouldReturn` Just ("test/t", "Helpers")

        it "is the component whose main-is it is, though another shares its directory" $
            ownerFor "test/Main.hs" `shouldReturn` Just ("test/u", "Main")

        it "is nothing outside every source directory" $ do
            cfg <- config (package ["library\n  hs-source-dirs: src"])

            ownerOf cfg (inProject "scripts/Build.hs") `shouldBe` Nothing
