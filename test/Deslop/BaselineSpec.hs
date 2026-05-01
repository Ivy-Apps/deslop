module Deslop.BaselineSpec (spec) where

import Deslop.Baseline (applyBaseline, loadBaselineFromFile)
import Deslop.Problem (Problem (..))
import Deslop.Rulebook (RuleId (..), RulebookId (..))
import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, runMockRoFileSystem)
import Effectful (IOE, runEff)
import Effects.FileSystem (AbsPath, absPathUnsafe, encodeOsPath)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.ModuleResolver (moduleIdUnsafe)

testPath :: AbsPath
testPath = absPathUnsafe (encodeOsPath "/test/baseline.yaml")

runTest :: MockRoFileSystem '[IOE] -> [Problem] -> IO [Problem]
runTest mocks problems = runEff . runMockRoFileSystem mocks $ do
    baseline <- loadBaselineFromFile testPath
    pure $ applyBaseline baseline problems

mockWithFile :: ByteString -> MockRoFileSystem '[IOE]
mockWithFile content =
    defaultMockRoFileSystem
        { mockFileExists = \_ -> pure True
        , mockReadFile = \_ -> pure content
        }

problemA :: Problem
problemA =
    RuleViolation
        { rulebook = RulebookId "rb"
        , rule = RuleId "rule"
        , badModule = moduleIdUnsafe "modA"
        , description = "problem A"
        , fix = "fix A"
        }

problemB :: Problem
problemB =
    RuleViolation
        { rulebook = RulebookId "rb"
        , rule = RuleId "rule"
        , badModule = moduleIdUnsafe "modB"
        , description = "problem B"
        , fix = "fix B"
        }

-- problemId for RuleViolation = rbId <> "#" <> rId <> "#" <> moduleId
-- so problemA's id = "rb#rule#modA", problemB's id = "rb#rule#modB"

spec :: Spec
spec = describe "Deslop.Baseline" $ do
    describe "load and apply baseline" $ do
        it "returns empty baseline when file does not exist" $ do
            let mocks = defaultMockRoFileSystem {mockFileExists = \_ -> pure False}
            result <- runTest mocks [problemA, problemB]
            result `shouldBe` [problemA, problemB]

        it "returns empty baseline for an empty YAML list" $ do
            result <- runTest (mockWithFile "[]\n") [problemA, problemB]
            result `shouldBe` [problemA, problemB]

        it "filters problems whose IDs are in the baseline" $ do
            let yaml = "- rb#rule#modA\n- rb#rule#modB\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` []

        it "filters only the matching problem and keeps the rest" $ do
            let yaml = "- rb#rule#modA\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` [problemB]

        it "returns empty baseline for invalid YAML" $ do
            let yaml = "not: valid: yaml: list\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` [problemA, problemB]
