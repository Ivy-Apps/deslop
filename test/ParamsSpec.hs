module ParamsSpec (spec) where

import Options.Applicative
import Params (Command (..), ParamsDto (..), paramsParser, parserPrefs)
import System.OsPath (osp)
import Test.Hspec

spec :: Spec
spec = describe "Params" $ do
    it "returns Nothing when no arguments given (command is required)" $ do
        parseParams [] `shouldBe` Nothing

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

    it "returns Nothing for an unknown command" $ do
        parseParams ["lint"] `shouldBe` Nothing
        parseParams ["run"] `shouldBe` Nothing

    it "returns Nothing for --help" $ do
        parseParams ["--help"] `shouldBe` Nothing

    it "returns Nothing for --version" $ do
        parseParams ["--version"] `shouldBe` Nothing

parseParams :: [String] -> Maybe ParamsDto
parseParams = getParseResult . execParserPure parserPrefs paramsParser
