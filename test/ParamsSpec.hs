module ParamsSpec (spec) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Effects.Git (GitRef (..))
import Options.Applicative
import Params (Command (..), Exploration (..), Opening (..), ParamsDto (..), paramsParser, parserPrefs)
import Paths_deslop (version)
import System.Exit (ExitCode (..))
import System.OsPath (osp)
import Test.Hspec

spec :: Spec
spec = describe "Params" $ do
    it "parses 'fix' command with default project dir" $
        parseParams ["fix"] `shouldBe` Just (ParamsDto {command = FixC, projectPath = [osp|.|]})

    it "parses 'check' command with default project dir" $
        parseParams ["check"] `shouldBe` Just (ParamsDto {command = CheckC, projectPath = [osp|.|]})

    it "parses 'baseline' command with default project dir" $
        parseParams ["baseline"] `shouldBe` Just (ParamsDto {command = BaselineC, projectPath = [osp|.|]})

    it "parses 'explore' command with an explicit project dir, measuring from main" $
        parseParams ["explore", "fixtures/app"]
            `shouldBe` Just (ParamsDto {command = exploreFrom "main", projectPath = [osp|fixtures/app|]})

    it "parses command with explicit PROJECT_DIR" $ do
        parseParams ["fix", "/some/ts/project"] `shouldBe` Just (ParamsDto {command = FixC, projectPath = [osp|/some/ts/project|]})
        parseParams ["check", "src"] `shouldBe` Just (ParamsDto {command = CheckC, projectPath = [osp|src|]})
        parseParams ["baseline", "."] `shouldBe` Just (ParamsDto {command = BaselineC, projectPath = [osp|.|]})

    it "parses explore's diff base" $
        parseParams ["explore", "--diff-base", "develop"]
            `shouldBe` Just (ParamsDto {command = exploreFrom "develop", projectPath = [osp|.|]})

    it "parses explore's --ci as never opening the page" $
        parseParams ["explore", "--ci"]
            `shouldBe` Just (ParamsDto {command = ExploreC Exploration {diffBase = GitRef "main", opening = NeverOpen}, projectPath = [osp|.|]})

    forM_ ["check", "fix", "baseline"] $ \cmd -> do
        it ("refuses --ci on " <> cmd) $
            fmap snd (parseOutput [cmd, "--ci"]) `shouldBe` Just (ExitFailure 1)

        it ("refuses a diff base on " <> cmd) $
            fmap snd (parseOutput [cmd, "--diff-base", "develop"]) `shouldBe` Just (ExitFailure 1)

    it "refuses a diff base git would read as an option" $ do
        fmap fst (parseOutput ["explore", "--diff-base=--output=x"])
            `shouldSatisfy` maybe False (isPrefixOf "option --diff-base: A ref cannot start with '-': --output=x")
        fmap fst (parseOutput ["explore", "--diff-base", "-x"]) `shouldSatisfy` isJust

    it "refuses an unknown command above the help" $ do
        parseOutput ["lint"] `shouldBe` Just ("Unknown command 'lint'. Expected: check, fix, baseline, or explore\n\n" <> helpText, ExitFailure 1)
        fmap snd (parseOutput ["run"]) `shouldBe` Just (ExitFailure 1)

    it "shows the help, failing, when given no arguments" $ do
        parseOutput [] `shouldBe` Just (helpText, ExitFailure 1)

    it "prints the help for --help, and succeeds" $ do
        parseOutput ["--help"] `shouldBe` Just (helpText, ExitSuccess)

    it "prints explore's help for explore --help, and succeeds" $ do
        parseOutput ["explore", "--help"] `shouldBe` Just (exploreHelpText, ExitSuccess)

    it "prints the version for --version, and succeeds" $ do
        parseOutput ["--version"] `shouldBe` Just ("Deslop Version " <> showVersion version, ExitSuccess)
        parseOutput ["-v"] `shouldBe` Just ("Deslop Version " <> showVersion version, ExitSuccess)
  where
    exploreFrom ref = ExploreC Exploration {diffBase = GitRef ref, opening = OpenWhenAttended}

parseParams :: [String] -> Maybe ParamsDto
parseParams = getParseResult . execParserPure parserPrefs paramsParser

-- | What the parser prints, and how it exits, when it stops short of a 'ParamsDto'.
parseOutput :: [String] -> Maybe (String, ExitCode)
parseOutput args = case execParserPure parserPrefs paramsParser args of
    Failure failure -> Just (renderFailure failure "deslop")
    Success _ -> Nothing
    CompletionInvoked _ -> Nothing

helpText :: String
helpText =
    toString . T.intercalate "\n" $
        [ "Deslop - A Haskell-powered code cleaner \x2728"
        , ""
        , "Usage: deslop [-v|--version] COMMAND"
        , ""
        , "  Removes slop from your project's architecture."
        , ""
        , "Available options:"
        , "  -h,--help                Show this help text"
        , "  -v,--version             Show version"
        , ""
        , "Available commands:"
        , "  check                    Report the problems in the project"
        , "  fix                      Repair the auto-fixable problems"
        , "  baseline                 Accept the current problems into deslop/baseline.yaml"
        , "  explore                  Write an interactive module graph to"
        , "                           deslop/diagram/index.html"
        ]

exploreHelpText :: String
exploreHelpText =
    toString . T.intercalate "\n" $
        [ "Usage: deslop explore [PROJECT_DIR] [--diff-base REF] [--ci]"
        , ""
        , "  Write an interactive module graph to deslop/diagram/index.html"
        , ""
        , "Available options:"
        , "  PROJECT_DIR              Path to the project (default: \".\")"
        , "  --diff-base REF          Mark the modules changed since this branch left REF"
        , "                           (origin/REF when the remote has it) (default: main)"
        , "  --ci                     Never open the page in a browser, even on a terminal"
        , "  -h,--help                Show this help text"
        , ""
        , "Global options:"
        , "  -v,--version             Show version"
        ]
