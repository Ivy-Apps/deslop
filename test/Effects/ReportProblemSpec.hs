module Effects.ReportProblemSpec (spec) where

import Effectful
import Effects.ReportProblem
import Test.Hspec

spec :: Spec
spec = describe "ReportProblem" $ do
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
                    Problem
                        { id = ProblemId "P001"
                        , location = Location {file = "src/Foo.ts", code = "x"}
                        , severity = Error
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

        it "getProblems returns problems in reverse order of reporting (newest first)" $ do
            let p1 =
                    Problem
                        { id = ProblemId "P1"
                        , location = Location {file = "a.ts", code = "1"}
                        , severity = Error
                        , description = "First"
                        , fix = "fix1"
                        }
            let p2 =
                    Problem
                        { id = ProblemId "P2"
                        , location = Location {file = "b.ts", code = "2"}
                        , severity = Error
                        , description = "Second"
                        , fix = "fix2"
                        }
            problems <-
                runEff
                    . runReportProblem
                    $ do
                        report p1
                        report p2
                        getProblems
            problems `shouldBe` [p2, p1]

        it "report does not affect the return value of the action" $ do
            let problem =
                    Problem
                        { id = ProblemId "P"
                        , location = Location {file = "f", code = "c"}
                        , severity = Error
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
