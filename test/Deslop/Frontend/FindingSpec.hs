module Deslop.Frontend.FindingSpec (spec) where

import Deslop.Frontend.Finding (Finding, applyFindings, fixable, problem, unfixable)
import Deslop.Module (Location (..))
import Deslop.Problem (LintRuleId (..), Problem (..), isAutoFixable)
import FileSystem.Path (encodeOsPath, relativePathUnsafe)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Frontend.Finding" $ do
    -- The two smart constructors are the only way to build a Finding, so these
    -- cover every Finding there is.
    it "a fixable Finding reports an auto-fixable Problem and repairs the unit" $ do
        let finding = fixable unfinished (Endo (+ 1))

        isAutoFixable (problem finding) `shouldBe` True
        repaired finding `shouldBe` 1

    it "an unfixable Finding reports a Problem that is not auto-fixable and leaves the unit alone" $ do
        let finding = unfixable unfinished

        isAutoFixable (problem finding) `shouldBe` False
        repaired finding `shouldBe` 0

-- | The unit, counting from zero, after the Finding's repair with nothing baselined.
repaired :: Finding Int -> Int
repaired = applyFindings (baselineOf []) 0 . pure

unfinished :: Bool -> Problem
unfinished autoFixable =
    LintProblem
        { lintRule = LintRuleId "no-unfinished"
        , location = Location {file = relativePathUnsafe (encodeOsPath "a.lines"), line = 1, code = "TODO"}
        , description = "Unfinished work."
        , fix = "Finish it."
        , autoFixable
        }
