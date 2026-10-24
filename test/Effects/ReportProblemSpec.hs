module Effects.ReportProblemSpec (spec) where

import Effectful
import Effects.ReportProblem
import Fixtures.Deslop.Problem (lintProblemAt, locatedAt)
import Test.Hspec

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
            let problem = lintProblemAt "P001" (locatedAt "src/Foo.ts" 1 "x")
            problems <-
                runEff
                    . runReportProblem
                    $ do
                        report problem
                        getProblems
            problems `shouldBe` [problem]

        it "getProblems returns the reported problems in order, whatever order they were reported in" $ do
            let p1 = lintProblemAt "P1" (locatedAt "a.ts" 1 "1")
            let p2 = lintProblemAt "P2" (locatedAt "b.ts" 1 "2")
            problems <-
                runEff
                    . runReportProblem
                    $ do
                        report p2
                        report p1
                        getProblems
            problems `shouldBe` [p1, p2]
