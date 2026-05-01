module Effects.ReportProblemSpec (spec) where

import Deslop.Problem (LintRuleId (..), Location (..), Problem (..))
import Effectful
import Effects.ReportProblem
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Effects.ReportProblem" $ do
    describe "runReportProblem" $ do
        it "returns the result of the action" $ do
            result <- runEff . runReportProblem $ pure (42 :: Int)
            result `shouldBe` 42

        it "getProblems returns empty list when nothing was reported" $ do
            problems <-
                runEff
                    . runReportProblem
                    $ getProblems
            problems `shouldBe` []

        it "getProblems returns a single problem after report" $ do
            let problem =
                    LintProblem
                        { lintRule = LintRuleId "P001"
                        , location = Location {file = rp "src/Foo.ts", code = "x"}
                        , description = "Something wrong"
                        , fix = "Do this"
                        }
            problems <-
                runEff
                    . runReportProblem
                    $ do
                        report problem
                        getProblems
            problems `shouldBe` [problem]

        it "getProblems returns the reported problems" $ do
            let p1 =
                    LintProblem
                        { lintRule = LintRuleId "P1"
                        , location = Location {file = rp "a.ts", code = "1"}
                        , description = "First"
                        , fix = "fix1"
                        }
            let p2 =
                    LintProblem
                        { lintRule = LintRuleId "P2"
                        , location = Location {file = rp "b.ts", code = "2"}
                        , description = "Second"
                        , fix = "fix2"
                        }
            problems <-
                runEff
                    . runReportProblem
                    $ do
                        report p2
                        report p1
                        getProblems
            problems `shouldBe` [p1, p2]

        it "report does not affect the return value of the action" $ do
            let problem =
                    LintProblem
                        { lintRule = LintRuleId "P"
                        , location = Location {file = rp "f", code = "c"}
                        , description = "desc"
                        , fix = "fix"
                        }
            result <-
                runEff
                    . runReportProblem
                    $ do
                        report problem
                        pure ("success" :: String)
            result `shouldBe` ("success" :: String)
