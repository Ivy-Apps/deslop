module ParamsSpec (spec) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Params (Command (..), ParamsDto (..), paramsParser, parserPrefs)
import Paths_deslop (version)
import System.Exit (ExitCode (..))
import System.OsPath (osp)
import Test.Hspec

spec :: Spec
spec = describe "Params" $ do
    it "parses 'fix' command with default project dir" $ do
        parseParams ["fix"]
            `shouldBe` Just
                ( ParamsDto
                    { command = FixC
                    , projectPath = [osp|.|]
                    }
                )

    it "parses 'check' command with default project dir" $ do
        parseParams ["check"]
            `shouldBe` Just
                ( ParamsDto
                    { command = CheckC
                    , projectPath = [osp|.|]
                    }
                )

    it "parses 'baseline' command with default project dir" $ do
        parseParams ["baseline"]
            `shouldBe` Just
                ( ParamsDto
                    { command = BaselineC
                    , projectPath = [osp|.|]
                    }
                )

    it "parses command with explicit PROJECT_DIR" $ do
        parseParams ["fix", "/some/ts/project"]
            `shouldBe` Just
                ( ParamsDto
                    { command = FixC
                    , projectPath = [osp|/some/ts/project|]
                    }
                )
        parseParams ["check", "src"]
            `shouldBe` Just
                ( ParamsDto
                    { command = CheckC
                    , projectPath = [osp|src|]
                    }
                )
        parseParams ["baseline", "."]
            `shouldBe` Just
                ( ParamsDto
                    { command = BaselineC
                    , projectPath = [osp|.|]
                    }
                )

    it "refuses an unknown command, naming the ones it knows, above the help" $ do
        parseOutput ["lint"] `shouldBe` Just ("Unknown command 'lint'. Expected: check, fix, or baseline\n\n" <> helpText, ExitFailure 1)
        fmap snd (parseOutput ["run"]) `shouldBe` Just (ExitFailure 1)

    it "shows the help, failing, when given no arguments" $ do
        parseOutput [] `shouldBe` Just (helpText, ExitFailure 1)

    it "prints the help for --help, and succeeds" $ do
        parseOutput ["--help"] `shouldBe` Just (helpText, ExitSuccess)

    it "prints the version for --version, and succeeds" $ do
        parseOutput ["--version"] `shouldBe` Just ("Deslop Version " <> showVersion version, ExitSuccess)
        parseOutput ["-v"] `shouldBe` Just ("Deslop Version " <> showVersion version, ExitSuccess)

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
        , "Usage: deslop [-v|--version] COMMAND [PROJECT_DIR]"
        , ""
        , "  Removes slop from your project's architecture. Commands: check, fix, baseline."
        , ""
        , "Available options:"
        , "  -h,--help                Show this help text"
        , "  -v,--version             Show version"
        , "  COMMAND                  Command to run: check, fix, or baseline"
        , "  PROJECT_DIR              Path to the project (default: \".\")"
        ]
