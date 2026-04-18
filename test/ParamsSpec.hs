module ParamsSpec (spec) where

import Options.Applicative
import Params (ParamsDto (..), paramsParser, parserPrefs)
import System.OsPath (osp)
import Test.Hspec

spec :: Spec
spec = describe "Params" $ do
    it "defaults project path to . and both flags to False when given no args" $ do
        parseParams []
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|.|]
                    , checkMode = False
                    }
                )

    it "parses PROJECT_PATH as sole positional argument" $ do
        parseParams ["/some/ts/project"]
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|/some/ts/project|]
                    , checkMode = False
                    }
                )
        parseParams ["."]
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|.|]
                    , checkMode = False
                    }
                )
        parseParams ["src"]
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|src|]
                    , checkMode = False
                    }
                )

    it "parses --check / -c and sets checkMode to True" $ do
        parseParams ["--check"]
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|.|]
                    , checkMode = True
                    }
                )
        parseParams ["-c"]
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|.|]
                    , checkMode = True
                    }
                )
        parseParams ["/path", "--check"]
            `shouldBe` Just
                ( ParamsDto
                    { projectPath = [osp|/path|]
                    , checkMode = True
                    }
                )

    it "returns Nothing for --help" $ do
        parseParams ["--help"] `shouldBe` Nothing

    it "returns Nothing for --version" $ do
        parseParams ["--version"] `shouldBe` Nothing

parseParams :: [String] -> Maybe ParamsDto
parseParams = getParseResult . execParserPure parserPrefs paramsParser
