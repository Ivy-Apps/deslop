module Deslop.BaselineSpec (spec) where

import Data.HashSet qualified as HS
import Deslop.Baseline (Baseline (..), applyBaseline, inBaseline, loadBaselineFromFile, saveBaseline)
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..), ViolationKind (..), problemId)
import Deslop.Rulebook (RuleId (..), RulebookId (..))
import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, mockFileAt, runMockRoFileSystem, runMockWrFileSystem)
import Effectful (IOE, runEff)
import Effects.FileSystem (AbsPath, absPathUnsafe, encodeOsPath, relativePathUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestUtils (requireJust)
import TypeScript.ModuleResolver (moduleIdUnsafe)

testPath :: AbsPath
testPath = absPathUnsafe (encodeOsPath "/test/baseline.yaml")

testProjectPath :: AbsPath
testProjectPath = absPathUnsafe (encodeOsPath "/test/project")

-- The path saveBaseline writes to: projectPath </> "deslop/baseline.yaml"
testBaselinePath :: AbsPath
testBaselinePath = absPathUnsafe (encodeOsPath "/test/project/deslop/baseline.yaml")

runTest :: MockRoFileSystem '[IOE] -> [Problem] -> IO [Problem]
runTest mocks problems = runEff . runMockRoFileSystem mocks $ do
    baseline <- loadBaselineFromFile testPath
    pure $ applyBaseline baseline problems

runLoadBaseline :: MockRoFileSystem '[IOE] -> IO Baseline
runLoadBaseline mocks = runEff . runMockRoFileSystem mocks $ loadBaselineFromFile testPath

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
        , prose = "problem A"
        , kind = MissingModule {requiredModule = moduleIdUnsafe "modA.spec"}
        , fix = "fix A"
        }

problemB :: Problem
problemB =
    RuleViolation
        { rulebook = RulebookId "rb"
        , rule = RuleId "rule"
        , badModule = moduleIdUnsafe "modB"
        , prose = "problem B"
        , kind = MissingModule {requiredModule = moduleIdUnsafe "modB.spec"}
        , fix = "fix B"
        }

-- problemId for RuleViolation = rbId <> "#" <> rId <> "#" <> moduleId
-- so problemA's id = "rb#rule#modA", problemB's id = "rb#rule#modB"
-- problemId for LintProblem = rId <> "#" <> filePath
-- so problemC's id = "lint-rule#src/file.ts"

problemC :: Problem
problemC =
    LintProblem
        { lintRule = LintRuleId "lint-rule"
        , location = Location {file = relativePathUnsafe (encodeOsPath "src/file.ts"), code = "bad code"}
        , description = "problem C"
        , fix = "fix C"
        , autoFixable = False
        }

spec :: Spec
spec = describe "Deslop.Baseline" $ do
    describe "load and apply baseline" $ do
        it "no baseline file -> passes all problems through" $ do
            let mocks = defaultMockRoFileSystem {mockFileExists = \_ -> pure False}
            result <- runTest mocks [problemA, problemB]
            result `shouldBe` [problemA, problemB]

        it "empty YAML list -> passes all problems through" $ do
            result <- runTest (mockWithFile "[]\n") [problemA, problemB]
            result `shouldBe` [problemA, problemB]

        it "all problems in baseline -> returns empty list" $ do
            let yaml = "- rb#rule#modA\n- rb#rule#modB\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` []

        it "partial baseline -> filters only matched problems" $ do
            let yaml = "- rb#rule#modA\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` [problemB]

        it "strips whitespaces in the baseline" $ do
            let yaml = "-  rb#rule#modA   \n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` [problemB]

        it "invalid YAML -> passes all problems through" $ do
            let yaml = "not: valid: yaml: list\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB]
            result `shouldBe` [problemA, problemB]

        it "LintProblem in baseline -> filters it out" $ do
            let yaml = "- lint-rule#src/file.ts\n"
            result <- runTest (mockWithFile yaml) [problemA, problemC]
            result `shouldBe` [problemA]

        it "baseline with comments -> still parses and filters correctly" $ do
            let yaml =
                    "# Known/accepted violations\n\
                    \# Format: {rulebook-id}#{rule-id}#{module-id}\n\
                    \- rb#rule#modA\n\
                    \# This one is intentionally kept:\n\
                    \# - rb#rule#modB\n\
                    \- lint-rule#src/file.ts\n"
            result <- runTest (mockWithFile yaml) [problemA, problemB, problemC]
            result `shouldBe` [problemB]

    describe "inBaseline" $ do
        it "problem present in baseline -> returns True" $ do
            let yaml = "- rb#rule#modA\n"
            baseline <- runLoadBaseline (mockWithFile yaml)
            inBaseline baseline problemA `shouldBe` True

        it "problem absent from baseline -> returns False" $ do
            let yaml = "- rb#rule#modA\n"
            baseline <- runLoadBaseline (mockWithFile yaml)
            inBaseline baseline problemB `shouldBe` False

        it "empty baseline -> always returns False" $ do
            baseline <- runLoadBaseline (mockWithFile "[]\n")
            inBaseline baseline problemA `shouldBe` False
            inBaseline baseline problemB `shouldBe` False
            inBaseline baseline problemC `shouldBe` False

        it "no baseline file -> always returns False" $ do
            let mocks = defaultMockRoFileSystem {mockFileExists = \_ -> pure False}
            baseline <- runLoadBaseline mocks
            inBaseline baseline problemA `shouldBe` False

        it "LintProblem present in baseline -> returns True" $ do
            let yaml = "- lint-rule#src/file.ts\n"
            baseline <- runLoadBaseline (mockWithFile yaml)
            inBaseline baseline problemC `shouldBe` True

        it "LintProblem absent from baseline -> returns False" $ do
            let yaml = "- rb#rule#modA\n"
            baseline <- runLoadBaseline (mockWithFile yaml)
            inBaseline baseline problemC `shouldBe` False

        it "multiple problems in baseline -> each matched independently" $ do
            let yaml = "- rb#rule#modA\n- rb#rule#modB\n"
            baseline <- runLoadBaseline (mockWithFile yaml)
            inBaseline baseline problemA `shouldBe` True
            inBaseline baseline problemB `shouldBe` True
            inBaseline baseline problemC `shouldBe` False

    describe "saveBaseline" $ do
        it "writes something to the baseline file path" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $ saveBaseline testProjectPath [problemA]
            written <- readIORef ref
            written `shouldSatisfy` isJust

        it "empty problem list -> writes empty YAML list" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $ saveBaseline testProjectPath []
            content <- requireJust "saveBaseline did not write" =<< readIORef ref
            baseline <-
                runEff . runMockRoFileSystem (mockFileAt testBaselinePath content) $
                    loadBaselineFromFile testBaselinePath
            applyBaseline baseline [problemA, problemB] `shouldBe` [problemA, problemB]

        it "saves RuleViolation problem ID and round-trips through load" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $ saveBaseline testProjectPath [problemA]
            content <- requireJust "saveBaseline did not write" =<< readIORef ref
            baseline <-
                runEff . runMockRoFileSystem (mockFileAt testBaselinePath content) $
                    loadBaselineFromFile testBaselinePath
            applyBaseline baseline [problemA, problemB] `shouldBe` [problemB]

        it "saves LintProblem ID and round-trips through load" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $ saveBaseline testProjectPath [problemC]
            content <- requireJust "saveBaseline did not write" =<< readIORef ref
            baseline <-
                runEff . runMockRoFileSystem (mockFileAt testBaselinePath content) $
                    loadBaselineFromFile testBaselinePath
            applyBaseline baseline [problemA, problemC] `shouldBe` [problemA]

        it "saves multiple problems and round-trips through load" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $ saveBaseline testProjectPath [problemA, problemB, problemC]
            content <- requireJust "saveBaseline did not write" =<< readIORef ref
            baseline <-
                runEff . runMockRoFileSystem (mockFileAt testBaselinePath content) $
                    loadBaselineFromFile testBaselinePath
            applyBaseline baseline [problemA, problemB, problemC] `shouldBe` []

        it "deduplicates the baseline" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $
                saveBaseline testProjectPath [problemA, problemB, problemA]
            content <- requireJust "saveBaseline did not write" =<< readIORef ref
            Baseline baseline <-
                runEff . runMockRoFileSystem (mockFileAt testBaselinePath content) $
                    loadBaselineFromFile testBaselinePath
            baseline `shouldBe` HS.fromList [problemId problemA, problemId problemB]

        it "the baseline is sorted" $ do
            ref <- newIORef Nothing
            runEff . runMockWrFileSystem ref $
                saveBaseline testProjectPath [problemC, problemA, problemB]
            content <- requireJust "saveBaseline did not write" =<< readIORef ref
            Baseline baseline <-
                runEff . runMockRoFileSystem (mockFileAt testBaselinePath content) $
                    loadBaselineFromFile testBaselinePath
            baseline
                `shouldBe` HS.fromList
                    [ problemId problemA
                    , problemId problemB
                    , problemId problemC
                    ]
