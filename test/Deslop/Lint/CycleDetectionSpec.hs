module Deslop.Lint.CycleDetectionSpec (spec) where

import Deslop.AST (AstModule)
import Deslop.Baseline (applyBaseline)
import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Lint.CycleDetection (noImportCycles)
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..), ProblemId (..), problemId)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Test.Hspec
import TestUtils (baselineOf, defaultTsConfig, mkModule, rp)
import TypeScript.Config (TsConfig)

runNoImportCycles :: [AstModule] -> IO [Problem]
runNoImportCycles modules =
    runEff
        . runReportProblem
        . runReader @TsConfig defaultTsConfig
        . runReader (buildModuleGraph modules)
        $ noImportCycles >> getProblems

cycleFix :: Text
cycleFix =
    "Import cycles are not allowed. Break the loop by removing one of its"
        <> " imports - usually by extracting the shared code into a module that"
        <> " both sides can depend on."

spec :: Spec
spec = describe "Deslop.Lint.CycleDetection" $ do
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
                            , description = "Circular dependency: a → b → a"
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
                            , description = "Circular dependency: a → b → c → a"
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
                            , description = "Circular dependency: a → a"
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
