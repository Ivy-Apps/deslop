module TypeScript.Config.LoaderSpec (spec) where

import Data.Text qualified as T
import Doubles.FileSystem (mockFileSystem, runMockRoFileSystem)
import Effectful (runEff)
import Effects.FileSystem (runFileSystemIO)
import FileSystem.Path (ProjectRoot (..), encodeOsPath)
import Hedgehog (Gen, evalIO, forAll, success)
import Hedgehog.Gen qualified as Gen
import System.OsPath (osp, (</>))
import Test.Hspec
import TestUtils (ap, mkAbsolute, pathSafeGolden, prop, requireRight)
import Text.Show.Pretty (ppShow)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..))
import TypeScript.Config.Loader (
    SkippedExtends (..),
    TsConfigLoadError (..),
    loadTsConfig,
    renderTsConfigLoadError,
 )

spec :: Spec
spec = describe "TypeScript.Config.Loader" $ do
    describe "resolving an extends chain" $ do
        it "reads a config that extends nothing" $ do
            cfg <- loadConfig [("/repo/tsconfig.json", aliasing "@")]

            aliases cfg `shouldBe` ["@/"]
            cfg.pathsBase `shouldBe` ap "/repo"

        it "inherits the paths of a relative base" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extending "./tsconfig.base.json")
                    , ("/repo/tsconfig.base.json", aliasing "@")
                    ]

            aliases cfg `shouldBe` ["@/"]

        it "appends .json to a base named without one" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extending "./tsconfig.base")
                    , ("/repo/tsconfig.base.json", aliasing "@")
                    ]

            aliases cfg `shouldBe` ["@/"]

        it "follows a base that lives above the project" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extending "../shared/base.json")
                    , ("/shared/base.json", aliasing "@")
                    ]

            aliases cfg `shouldBe` ["@/"]

        it "follows a rooted base path" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extending "/shared/base.json")
                    , ("/shared/base.json", aliasing "@")
                    ]

            aliases cfg `shouldBe` ["@/"]

        it "gives a later entry of an array precedence over an earlier one" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extendingAll ["./first.json", "./second.json"])
                    , ("/repo/first.json", aliasing "@first")
                    , ("/repo/second.json", aliasing "@second")
                    ]

            aliases cfg `shouldBe` ["@second/"]

        it "lets a config's own paths replace the ones it extends" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extendingWithAlias "./base.json" "@own")
                    , ("/repo/base.json", aliasing "@base")
                    ]

            aliases cfg `shouldBe` ["@own/"]

        it "treats two branches extending one base as a diamond, not a cycle" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extendingAll ["./left.json", "./right.json"])
                    , ("/repo/left.json", extending "./base.json")
                    , ("/repo/right.json", extending "./base.json")
                    , ("/repo/base.json", aliasing "@")
                    ]

            aliases cfg `shouldBe` ["@/"]

    describe "the base that paths resolve against" $ do
        it "is the directory of the config that declared the paths" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extending "../shared/base.json")
                    , ("/shared/base.json", aliasing "@")
                    ]

            cfg.pathsBase `shouldBe` ap "/shared"

        it "is a declared baseUrl, relative to the config that declared it" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extending "../shared/base.json")
                    , ("/shared/base.json", aliasingUnder "./src" "@")
                    ]

            cfg.pathsBase `shouldBe` ap "/shared/src"

        it "prefers a baseUrl to the directory the paths came from" $ do
            cfg <-
                loadConfig
                    [ ("/repo/tsconfig.json", extendingUnder "../shared/base.json" "./src")
                    , ("/shared/base.json", aliasing "@")
                    ]

            aliases cfg `shouldBe` ["@/"]
            cfg.pathsBase `shouldBe` ap "/repo/src"

    describe "a chain that cannot be loaded" $ do
        it "reports a config that extends itself" $ do
            res <-
                load
                    [ ("/repo/tsconfig.json", extending "./a.json")
                    , ("/repo/a.json", extending "./tsconfig.json")
                    ]

            res
                `shouldBe` Left
                    ( TsConfigCycle
                        (ap "/repo/tsconfig.json")
                        [ap "/repo/tsconfig.json", ap "/repo/a.json"]
                    )

        it "reports a base that is not there, and how it was reached" $ do
            res <- load [("/repo/tsconfig.json", extending "./missing.json")]

            res
                `shouldBe` Left
                    (TsConfigUnreadable (ap "/repo/missing.json") [ap "/repo/tsconfig.json"])

        it "reports a base it cannot parse" $ do
            res <-
                load
                    [ ("/repo/tsconfig.json", extending "./base.json")
                    , ("/repo/base.json", "{\"invalid\"}")
                    ]

            res `shouldSatisfy` \case
                Left (TsConfigUnparseable path chain _) ->
                    (path, chain) == (ap "/repo/base.json", [ap "/repo/tsconfig.json"])
                _ -> False

        it "reports an extends that is neither a string nor an array of them" $ do
            res <- load [("/repo/tsconfig.json", "{\"extends\": 42}")]

            res `shouldBe` Left (TsConfigExtendsNotText (ap "/repo/tsconfig.json") [])

    describe "an extends Deslop does not resolve" $ do
        it "skips a package specifier and keeps the rest of the config" $ do
            res <-
                load
                    [("/repo/tsconfig.json", extendingWithAlias "@repo/typescript-config/base.json" "@")]

            (cfg, skipped) <- requireRight (T.unpack . renderTsConfigLoadError mockRepoRoot) res
            aliases cfg `shouldBe` ["@/"]
            skipped
                `shouldBe` [ SkippedExtends
                                { specifier = "@repo/typescript-config/base.json"
                                , inFile = ap "/repo/tsconfig.json"
                                }
                           ]

    describe "properties" $ do
        prop "answers for any extends graph, cyclic ones included" $ do
            files <- forAll genExtendsGraph

            _ <- evalIO . load $ files

            success

    describe "loading a config off disk" $ do
        let cases =
                [ "simple.json"
                , "invalid.json"
                , "complex.json"
                , "minimal.json"
                , "base-url.json"
                , "sorting-and-comments.json"
                , "extends.json"
                ]
        forM_ cases $ \file ->
            it file $ do
                cfgPath <- mkAbsolute ([osp|fixtures/typescript/config|] </> encodeOsPath (T.pack file))
                res <- runEff . runFileSystemIO $ loadTsConfig cfgPath
                pathSafeGolden ("loadTsConfig-" <> file) (ppShow res)

--------------------------------------------------------------------------------
-- Running the loader
--------------------------------------------------------------------------------

load :: [(Text, Text)] -> IO (Either TsConfigLoadError (TsConfig, [SkippedExtends]))
load files =
    runEff
        . runMockRoFileSystem (mockFileSystem (bimap ap encodeUtf8 <$> files))
        . loadTsConfig
        $ ap "/repo/tsconfig.json"

-- | The root the in-memory filesystem's @\/repo@ project is rooted at.
mockRepoRoot :: ProjectRoot
mockRepoRoot = ProjectRoot (ap "/repo")

loadConfig :: [(Text, Text)] -> IO TsConfig
loadConfig files = do
    res <- load files
    fst <$> requireRight (T.unpack . renderTsConfigLoadError mockRepoRoot) res

-- | The alias each path mapping is keyed by, in the order the resolver tries them.
aliases :: TsConfig -> [Text]
aliases = fmap (aliasOf . (.pattern) . (.key)) . (.paths)
  where
    aliasOf (Exact t) = t
    aliasOf (Wildcard pre _) = pre

--------------------------------------------------------------------------------
-- Config files
--------------------------------------------------------------------------------

aliasing :: Text -> Text
aliasing alias = "{\"compilerOptions\": " <> options alias <> "}"

aliasingUnder :: Text -> Text -> Text
aliasingUnder baseUrl alias =
    "{\"compilerOptions\": {\"baseUrl\": \"" <> baseUrl <> "\", " <> mappings alias <> "}}"

extending :: Text -> Text
extending target = "{\"extends\": \"" <> target <> "\"}"

extendingAll :: [Text] -> Text
extendingAll targets =
    "{\"extends\": [" <> T.intercalate ", " (quoted <$> targets) <> "]}"

extendingWithAlias :: Text -> Text -> Text
extendingWithAlias target alias =
    "{\"extends\": \"" <> target <> "\", \"compilerOptions\": " <> options alias <> "}"

extendingUnder :: Text -> Text -> Text
extendingUnder target baseUrl =
    "{\"extends\": \""
        <> target
        <> "\", \"compilerOptions\": {\"baseUrl\": \""
        <> baseUrl
        <> "\"}}"

options :: Text -> Text
options alias = "{" <> mappings alias <> "}"

mappings :: Text -> Text
mappings alias = "\"paths\": {\"" <> alias <> "/*\": [\"./src/*\"]}"

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

{- | A handful of configs each extending an arbitrary subset of the others, so
that cycles, diamonds and dangling links all occur.
-}
genExtendsGraph :: Gen [(Text, Text)]
genExtendsGraph = do
    root <- ("/repo/tsconfig.json",) <$> genConfig
    rest <- traverse (\n -> ("/repo/" <> n <> ".json",) <$> genConfig) names
    pure $ root : rest
  where
    names = ["a", "b", "c"]
    targets = "./tsconfig.json" : (("./" <>) . (<> ".json") <$> names)
    genConfig = extendingAll <$> Gen.subsequence targets
