module ParamsSpec where

import Options.Applicative
import Params (Params (..), optsInfo, parserPrefs)
import Test.Hspec

spec :: Spec
spec = describe "parseParams" $ do
    it "defaults project path to . and both flags to False when given no args" $ do
        parseParams []
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , modifiedOnly = False
                    , checkMode = False
                    }
                )

    it "parses PROJECT_PATH as sole positional argument" $ do
        parseParams ["/some/ts/project"]
            `shouldBe` Just
                ( Params
                    { projectPath = "/some/ts/project"
                    , modifiedOnly = False
                    , checkMode = False
                    }
                )
        parseParams ["."]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , modifiedOnly = False
                    , checkMode = False
                    }
                )
        parseParams ["src"]
            `shouldBe` Just
                ( Params
                    { projectPath = "src"
                    , modifiedOnly = False
                    , checkMode = False
                    }
                )

    it "parses --modified / -m and sets modifiedOnly to True" $ do
        parseParams ["--modified"]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , modifiedOnly = True
                    , checkMode = False
                    }
                )
        parseParams ["-m"]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , modifiedOnly = True
                    , checkMode = False
                    }
                )
        parseParams ["/path", "--modified"]
            `shouldBe` Just
                ( Params
                    { projectPath = "/path"
                    , modifiedOnly = True
                    , checkMode = False
                    }
                )
        parseParams ["--modified", "other/dir"]
            `shouldBe` Just
                ( Params
                    { projectPath = "other/dir"
                    , modifiedOnly = True
                    , checkMode = False
                    }
                )

    it "parses --check / -c and sets checkMode to True" $ do
        parseParams ["--check"]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , modifiedOnly = False
                    , checkMode = True
                    }
                )
        parseParams ["-c"]
            `shouldBe` Just
                ( Params
                    { projectPath = "."
                    , modifiedOnly = False
                    , checkMode = True
                    }
                )
        parseParams ["/path", "--check"]
            `shouldBe` Just
                ( Params
                    { projectPath = "/path"
                    , modifiedOnly = False
                    , checkMode = True
                    }
                )

    it "parses combination of path, --modified and --check" $ do
        parseParams ["my-project", "--modified", "--check"]
            `shouldBe` Just
                ( Params
                    { projectPath = "my-project"
                    , modifiedOnly = True
                    , checkMode = True
                    }
                )
        parseParams ["my-project", "-m", "-c"]
            `shouldBe` Just
                ( Params
                    { projectPath = "my-project"
                    , modifiedOnly = True
                    , checkMode = True
                    }
                )
        parseParams ["--modified", "--check", "lib"]
            `shouldBe` Just
                ( Params
                    { projectPath = "lib"
                    , modifiedOnly = True
                    , checkMode = True
                    }
                )

    it "returns Nothing for --help" $ do
        parseParams ["--help"] `shouldBe` Nothing

    it "returns Nothing for --version" $ do
        parseParams ["--version"] `shouldBe` Nothing

parseParams :: [String] -> Maybe Params
parseParams = getParseResult . execParserPure parserPrefs optsInfo
