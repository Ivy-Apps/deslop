module UISpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Types (ModuleCount (..), RuleCount (..), RunSummary (..))
import UI (coverage, summaryLine)

spec :: Spec
spec = describe "UI" $ do
    describe "coverage" $ do
        it "names what a check went through" $
            coverage (Checked (ModuleCount 412) (RuleCount 38))
                `shouldBe` "Checked 412 modules enforcing 38 rules"

        it "names what a baseline went through" $
            coverage (Baselined (ModuleCount 412) (RuleCount 38))
                `shouldBe` "Baselined 412 modules enforcing 38 rules"

        it "omits the rules a fix never enforces" $
            coverage (Scanned (ModuleCount 412))
                `shouldBe` "Scanned 412 modules"

        it "singularises both counts" $
            coverage (Checked (ModuleCount 1) (RuleCount 1))
                `shouldBe` "Checked 1 module enforcing 1 rule"

        it "reports a project with nothing in it" $
            coverage (Checked (ModuleCount 0) (RuleCount 0))
                `shouldBe` "Checked 0 modules enforcing 0 rules"

    describe "summaryLine" $ do
        it "renders sub-second runs in whole milliseconds" $
            summaryLine (Checked (ModuleCount 412) (RuleCount 38)) 0.87
                `shouldBe` "⏱  Checked 412 modules enforcing 38 rules in 870ms"

        it "rounds fractional milliseconds" $
            summaryLine (Scanned (ModuleCount 2)) 0.0004
                `shouldBe` "⏱  Scanned 2 modules in 0ms"

        it "renders runs of a second or more in seconds" $
            summaryLine (Scanned (ModuleCount 2)) 1.5
                `shouldBe` "⏱  Scanned 2 modules in 1.50s"

        it "keeps two decimals for long runs" $
            summaryLine (Baselined (ModuleCount 9) (RuleCount 3)) 123.456
                `shouldBe` "⏱  Baselined 9 modules enforcing 3 rules in 123.46s"
