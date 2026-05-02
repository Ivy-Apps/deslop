module Deslop.ProblemFormatterSpec (spec) where

import Deslop.Problem (LintRuleId (LintRuleId), Location (..), Problem (..))
import Deslop.ProblemFormatter (formatProblem)
import Deslop.Rulebook (RuleId (RuleId), RulebookId (RulebookId))
import Effects.FileSystem (encodeOsPath, relativePathUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.ModuleResolver (moduleIdUnsafe)

lintProblem :: Problem
lintProblem =
    LintProblem
        { lintRule = LintRuleId "no-relative-imports"
        , location =
            Location
                { file = relativePathUnsafe (encodeOsPath "src/Foo.ts")
                , code = "   import {bar} from './bar'   \n"
                }
        , description = "No relative imports allowed"
        , fix = "Use absolute imports"
        , autoFixable = False
        }

ruleViolation :: Problem
ruleViolation =
    RuleViolation
        { rulebook = RulebookId "architecture"
        , rule = RuleId "no-barrel-imports"
        , badModule = moduleIdUnsafe "@/lib/util"
        , description = "Barrel imports are forbidden"
        , fix = "Import directly from the module"
        }

spec :: Spec
spec = describe "Deslop.ProblemFormatter" $ do
    describe "formatProblem" $ do
        describe "LintProblem" $ do
            it "formats header with problem id" $ do
                let result = formatProblem lintProblem
                result
                    `shouldBe` "# no-relative-imports#src/Foo.ts\n"
                        <> "No relative imports allowed\n"
                        <> "```ts\nimport {bar} from './bar'\n```\n"
                        <> "FIX: Use absolute imports"

            it "strips whitespace from fix" $ do
                let p = lintProblem {fix = "  Use absolute imports  "}
                let result = formatProblem p
                result
                    `shouldBe` "# no-relative-imports#src/Foo.ts\n"
                        <> "No relative imports allowed\n"
                        <> "```ts\nimport {bar} from './bar'\n```\n"
                        <> "FIX: Use absolute imports"

            it "prefixes [AUTO-FIXABLE] when autoFixable is True" $ do
                let p =
                        LintProblem
                            { lintRule = LintRuleId "no-relative-imports"
                            , location =
                                Location
                                    { file = relativePathUnsafe (encodeOsPath "src/Foo.ts")
                                    , code = "   import {bar} from './bar'   \n"
                                    }
                            , description = "No relative imports allowed"
                            , fix = "Use absolute imports"
                            , autoFixable = True
                            }
                let result = formatProblem p
                result
                    `shouldBe` "[AUTO-FIXABLE] # no-relative-imports#src/Foo.ts\n"
                        <> "No relative imports allowed\n"
                        <> "```ts\nimport {bar} from './bar'\n```\n"
                        <> "FIX: Use absolute imports"

        describe "RuleViolation" $ do
            it "formats header with problem id" $ do
                let result = formatProblem ruleViolation
                result
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n"
                        <> "FIX: Import directly from the module"

            it "strips whitespace from fix" $ do
                let p = ruleViolation {fix = "  Import directly from the module  "}
                let result = formatProblem p
                result
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n"
                        <> "FIX: Import directly from the module"
