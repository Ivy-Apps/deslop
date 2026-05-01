module Deslop.ProblemSpec (spec) where

import Deslop.Problem (LintRuleId (LintRuleId), Location (..), Problem (..), ProblemId (..), problemId)
import Deslop.Rulebook (RuleId (RuleId), RulebookId (RulebookId))
import Effects.FileSystem (encodeOsPath, relativePathUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.ModuleResolver (moduleIdUnsafe)

spec :: Spec
spec = describe "Deslop.Problem" $ do
    describe "problemId" $ do
        it "lint problem id" $ do
            let p =
                    LintProblem
                        { lintRule = LintRuleId "no-relative-imports"
                        , location =
                            Location
                                { file = relativePathUnsafe (encodeOsPath "src/Foo.ts")
                                , code = "import {bar} from './bar'"
                                }
                        , description = "No relative imports allowed"
                        , fix = "Use absolute imports"
                        }
            problemId p `shouldBe` ProblemId "no-relative-imports#src/Foo.ts"

        it "rule violation id" $ do
            let p =
                    RuleViolation
                        { rulebook = RulebookId "architecture"
                        , rule = RuleId "no-barrel-imports"
                        , badModule = moduleIdUnsafe "@/lib/util"
                        , description = "Barrel imports are forbidden"
                        , fix = "Import directly from the module"
                        }
            problemId p `shouldBe` ProblemId "architecture#no-barrel-imports#@/lib/util"
