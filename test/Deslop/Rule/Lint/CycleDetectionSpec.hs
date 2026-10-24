module Deslop.Rule.Lint.CycleDetectionSpec (spec) where

import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Module (Location (..), Module)
import Deslop.Problem (Problem (..), ProblemId (..), problemId)
import Deslop.Problem.Baseline (applyBaseline)
import Deslop.Rule.Lint.CycleDetection (noImportCycles)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Fixtures.Deslop.Module (mkModule, mkModuleAt, withUnresolved)
import Fixtures.Deslop.Problem (describedAs, lintProblemAt, locatedAt, withFix)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Test.Hspec
import TestUtils (rp)

runNoImportCycles :: [Module] -> IO [Problem]
runNoImportCycles modules =
    runEff
        . runReportProblem
        . runReader (buildModuleGraph modules)
        $ noImportCycles >> getProblems

-- | The cycle reported from @a.ts@, entered by the statement, through the modules.
cycleThrough :: Text -> Text -> Problem
cycleThrough code modules =
    withFix cycleFix
        . describedAs ("Circular dependency (import cycle) detected: " <> modules)
        . lintProblemAt "no-import-cycles"
        $ locatedAt "a.ts" 1 code

cycleFix :: Text
cycleFix =
    "Import cycles are not allowed. Break the loop by removing one of its"
        <> " imports - usually by extracting the shared code into a module that"
        <> " both sides can depend on."

spec :: Spec
spec = describe "Deslop.Rule.Lint.CycleDetection" $ do
    -- What an unresolved dependency points at is unknown, so a loop through
    -- it is one nobody knows is there.
    it "ignores a dependency nobody can name, even one spelled like its importer" $ do
        let a = mkModule "a" ["b"]
            b = withUnresolved "a" $ mkModule "b" []
        runNoImportCycles [a, b] `shouldReturn` []

    it "reports a two-module cycle" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["a"]
        problems <- runNoImportCycles [a, b]
        problems
            `shouldBe` [cycleThrough "import { ... } from 'b'" "a → b → a"]

    it "reports a three-module cycle" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["c"]
            c = mkModule "c" ["a"]
        problems <- runNoImportCycles [a, b, c]
        problems
            `shouldBe` [cycleThrough "import { ... } from 'b'" "a → b → c → a"]

    it "reports a module importing itself" $ do
        let a = mkModule "a" ["a"]
        problems <- runNoImportCycles [a]
        problems
            `shouldBe` [cycleThrough "import { ... } from 'a'" "a → a"]

    it "reports nothing for an acyclic graph" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" []
        runNoImportCycles [a, b] `shouldReturn` []

    it "keys the problem on the rule id and the start module's file" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["a"]
        problems <- runNoImportCycles [a, b]
        map problemId problems `shouldBe` [ProblemId "no-import-cycles#a.ts"]

    it "is suppressed when the start module is baselined" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["a"]
        problems <- runNoImportCycles [a, b]
        applyBaseline (baselineOf ["no-import-cycles#a.ts"]) problems `shouldBe` []

    it "is not suppressed by baselining a module that is not the start" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["a"]
        problems <- runNoImportCycles [a, b]
        applyBaseline (baselineOf ["no-import-cycles#b.ts"]) problems `shouldBe` problems

    it "points at the file the cycle is entered from" $ do
        let a = mkModuleAt "src/a.ts" "a" ["b"]
            b = mkModuleAt "src/b.ts" "b" ["a"]
        problems <- runNoImportCycles [a, b]
        map (.location.file) problems `shouldBe` [rp "src/a.ts"]
