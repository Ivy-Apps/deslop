module ParamsSpec (spec) where

import Options.Applicative
import Params (Params (..), paramsParser, parserPrefs)
import Test.Hspec

spec :: Spec
spec = describe "parseParams" $ do
    it "defaults project path to . and both flags to False when given no args" $ do
        parseParams []
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , checkMode = False
                    }
                )

    it "parses PROJECT_PATH as sole positional argument" $ do
        parseParams ["/some/ts/project"]
            `shouldBe` Just
                ( Params
                    { projectPath = "/some/ts/project"
                    , checkMode = False
                    }
                )
        parseParams ["."]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , checkMode = False
                    }
                )
        parseParams ["src"]
            `shouldBe` Just
                ( Params
                    { projectPath = "src"
                    , checkMode = False
                    }
                )

    it "parses --check / -c and sets checkMode to True" $ do
        parseParams ["--check"]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , checkMode = True
                    }
                )
        parseParams ["-c"]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , checkMode = True
                    }
                )
        parseParams ["/path", "--check"]
            `shouldBe` Just
                ( Params
                    { projectPath = "/path"
                    , checkMode = True
                    }
                )

    it "returns Nothing for --help" $ do
        parseParams ["--help"] `shouldBe` Nothing

    it "returns Nothing for --version" $ do
        parseParams ["--version"] `shouldBe` Nothing

parseParams :: [String] -> Maybe Params
parseParams = getParseResult . execParserPure parserPrefs paramsParser
