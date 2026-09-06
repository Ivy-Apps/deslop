module Deslop.Rule.Lint.CycleDetectionSpec (spec) where

import Deslop.AST (AstModule)
import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..), ProblemId (..), problemId)
import Deslop.Problem.Baseline (applyBaseline)
import Deslop.Rule.Lint.CycleDetection (noImportCycles)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import FileSystem.Path (ProjectRoot (..))
import Fixtures.Deslop.AST (mkModule, mkModuleAt)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Test.Hspec
import TestUtils (ap, rp)

-- | The root 'Fixtures.Deslop.AST.mkModule' places its source files under.
repoRoot :: ProjectRoot
repoRoot = ProjectRoot (ap "/home/repo")

runNoImportCycles :: [AstModule] -> IO [Problem]
runNoImportCycles = runNoImportCyclesIn repoRoot

runNoImportCyclesIn :: ProjectRoot -> [AstModule] -> IO [Problem]
runNoImportCyclesIn root modules =
    runEff
        . runReportProblem
        . runReader @ProjectRoot root
        . runReader (buildModuleGraph modules)
        $ noImportCycles >> getProblems

cycleFix :: Text
cycleFix =
    "Import cycles are not allowed. Break the loop by removing one of its"
        <> " imports - usually by extracting the shared code into a module that"
        <> " both sides can depend on."

spec :: Spec
spec = describe "Deslop.Rule.Lint.CycleDetection" $ do
    it "reports a two-module cycle" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["a"]
        problems <- runNoImportCycles [a, b]
        problems
            `shouldBe` [ LintProblem
                            { lintRule = LintRuleId "no-import-cycles"
                            , location =
                                Location
                                    { file = rp "a.ts"
                                    , code = "import { ... } from 'b'"
                                    }
                            , description = "Circular dependency (import cycle) detected: a → b → a"
                            , fix = cycleFix
                            , autoFixable = False
                            }
                       ]

    it "reports a three-module cycle" $ do
        let a = mkModule "a" ["b"]
            b = mkModule "b" ["c"]
            c = mkModule "c" ["a"]
        problems <- runNoImportCycles [a, b, c]
        problems
            `shouldBe` [ LintProblem
                            { lintRule = LintRuleId "no-import-cycles"
                            , location =
                                Location
                                    { file = rp "a.ts"
                                    , code = "import { ... } from 'b'"
                                    }
                            , description = "Circular dependency (import cycle) detected: a → b → c → a"
                            , fix = cycleFix
                            , autoFixable = False
                            }
                       ]

    it "reports a module importing itself" $ do
        let a = mkModule "a" ["a"]
        problems <- runNoImportCycles [a]
        problems
            `shouldBe` [ LintProblem
                            { lintRule = LintRuleId "no-import-cycles"
                            , location =
                                Location
                                    { file = rp "a.ts"
                                    , code = "import { ... } from 'a'"
                                    }
                            , description = "Circular dependency (import cycle) detected: a → a"
                            , fix = cycleFix
                            , autoFixable = False
                            }
                       ]

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

    it "reports a file outside the project root by going back out of it" $ do
        -- makeRelative would hand the absolute path straight back here, and a
        -- Problem Id carrying one matches nothing on another checkout.
        let a = mkModuleAt "/home/other/a.ts" "a" ["b"]
            b = mkModuleAt "/home/other/b.ts" "b" ["a"]
        problems <- runNoImportCyclesIn repoRoot [a, b]
        map (.location.file) problems `shouldBe` [rp "../other/a.ts"]
