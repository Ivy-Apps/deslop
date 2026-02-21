module ParamsSpec where

import Params (Params (..), optsInfo, parserPrefs)
import Test.Hspec
import Options.Applicative

spec :: Spec
spec = describe "parseParams" $ do
    it "defaults project path to . and both flags to False when given no args" $ do
        parseParams [] `shouldBe` Just (Params "." False False)

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
            `shouldBe` Just (Params "." False False)
        parseParams ["src"]
            `shouldBe` Just (Params "src" False False)

    it "parses --modified / -m and sets modifiedOnly to True" $ do
        parseParams ["--modified"]
            `shouldBe` Just (Params "." True False)
        parseParams ["-m"]
            `shouldBe` Just (Params "." True False)
        parseParams ["/path", "--modified"]
            `shouldBe` Just (Params "/path" True False)
        parseParams ["--modified", "other/dir"]
            `shouldBe` Just (Params "other/dir" True False)

    it "parses --check / -c and sets checkMode to True" $ do
        parseParams ["--check"]
            `shouldBe` Just (Params "." False True)
        parseParams ["-c"]
            `shouldBe` Just (Params "." False True)
        parseParams ["/path", "--check"]
            `shouldBe` Just (Params "/path" False True)

    it "parses combination of path, --modified and --check" $ do
        parseParams ["my-project", "--modified", "--check"]
            `shouldBe` Just (Params "my-project" True True)
        parseParams ["my-project", "-m", "-c"]
            `shouldBe` Just (Params "my-project" True True)
        parseParams ["--modified", "--check", "lib"]
            `shouldBe` Just (Params "lib" True True)

    it "returns Nothing for --help" $ do
        parseParams ["--help"] `shouldBe` Nothing

    it "returns Nothing for --version" $ do
        parseParams ["--version"] `shouldBe` Nothing

parseParams :: [String] -> Maybe Params
parseParams = getParseResult . execParserPure parserPrefs optsInfo
