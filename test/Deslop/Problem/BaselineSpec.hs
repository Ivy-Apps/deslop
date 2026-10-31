module Deslop.Problem.BaselineSpec (spec) where

import Data.HashSet qualified as HS
import Data.Map.Strict qualified as Map
import Deslop.Module (moduleNameUnsafe)
import Deslop.Problem (Problem (..), ViolationKind (..), problemId)
import Deslop.Problem.Baseline (Baseline (..), applyBaseline, inBaseline, loadBaseline, loadBaselineFromFile, saveBaseline)
import Doubles.FileSystem (InMemoryFileSystem (..), MockRoFileSystem, mockFileAt, mockFiles, newInMemoryFileSystem, runInMemoryFileSystem, runReadOnly)
import Effectful (IOE, runEff)
import Effects.FileSystem (runFileSystemIO)
import FileSystem.Path (AbsPath)
import Fixtures.Deslop.Problem (brokenRule, lintProblemAt, locatedAt, violationOf)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (ap, inEmptyDir, mkAbsolute)

testPath :: AbsPath
testPath = ap "/test/baseline.yaml"

testProjectPath :: AbsPath
testProjectPath = ap "/test/project"

-- The path saveBaseline writes to: projectPath </> "deslop/baseline.yaml"
testBaselinePath :: AbsPath
testBaselinePath = ap "/test/project/deslop/baseline.yaml"

runTest :: MockRoFileSystem '[IOE] -> [Problem] -> IO [Problem]
runTest mocks problems = flip applyBaseline problems <$> runLoadBaseline mocks

runLoadBaseline :: MockRoFileSystem '[IOE] -> IO Baseline
runLoadBaseline mocks = runReadOnly mocks $ loadBaselineFromFile testPath

mockWithFile :: ByteString -> MockRoFileSystem '[IOE]
mockWithFile = mockFileAt testPath

{- | Saves the problems as a Baseline, then reads the project's Baseline back
the way the next run will: what is on disk afterwards, and what it means.
-}
saveThenLoad :: [Problem] -> IO (Map AbsPath ByteString, Baseline)
saveThenLoad problems = do
    fs <- newInMemoryFileSystem []
    baseline <-
        runEff . runInMemoryFileSystem fs $
            saveBaseline testProjectPath problems >> loadBaseline testProjectPath
    (,baseline) <$> readIORef fs.files

-- problemA's id is "rb#rule#modA", problemB's is "rb#rule#modB" and
-- problemC's is "lint-rule#src/file.ts".

problemA :: Problem
problemA = violationOf (brokenRule "rb" "rule") "modA" MissingModule {requiredModule = moduleNameUnsafe "modA.spec"}

problemB :: Problem
problemB = violationOf (brokenRule "rb" "rule") "modB" MissingModule {requiredModule = moduleNameUnsafe "modB.spec"}

problemC :: Problem
problemC = lintProblemAt "lint-rule" (locatedAt "src/file.ts" 1 "bad code")

spec :: Spec
spec = describe "Deslop.Problem.Baseline" $ do
    describe "load and apply baseline" $ do
        it "no baseline file -> passes all problems through" $ do
            result <- runTest (mockFiles []) [problemA, problemB]
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
            baseline <- runLoadBaseline (mockFiles [])
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
        it "writes every id, sorted and one per line, to the project's deslop/baseline.yaml" $ do
            (files, _) <- saveThenLoad [problemC, problemA]

            files `shouldBe` Map.singleton testBaselinePath "- lint-rule#src/file.ts\n- rb#rule#modA\n"

        it "creates the deslop directory of a project that has none" . inEmptyDir $ \tmp -> do
            project <- mkAbsolute tmp

            baseline <- runEff . runFileSystemIO $ saveBaseline project [problemA] >> loadBaseline project

            applyBaseline baseline [problemA, problemB] `shouldBe` [problemB]

        it "empty problem list -> writes empty YAML list" $ do
            (files, baseline) <- saveThenLoad []

            Map.lookup testBaselinePath files `shouldBe` Just "[]\n"
            applyBaseline baseline [problemA, problemB] `shouldBe` [problemA, problemB]

        it "saves RuleViolation problem ID and round-trips through load" $ do
            (_, baseline) <- saveThenLoad [problemA]

            applyBaseline baseline [problemA, problemB] `shouldBe` [problemB]

        it "saves LintProblem ID and round-trips through load" $ do
            (_, baseline) <- saveThenLoad [problemC]

            applyBaseline baseline [problemA, problemC] `shouldBe` [problemA]

        it "saves multiple problems and round-trips through load" $ do
            (_, baseline) <- saveThenLoad [problemA, problemB, problemC]

            applyBaseline baseline [problemA, problemB, problemC] `shouldBe` []

        it "deduplicates the baseline" $ do
            (files, Baseline baseline) <- saveThenLoad [problemA, problemB, problemA]

            baseline `shouldBe` HS.fromList [problemId problemA, problemId problemB]
            Map.lookup testBaselinePath files `shouldBe` Just "- rb#rule#modA\n- rb#rule#modB\n"
