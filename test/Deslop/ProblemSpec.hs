module Deslop.ProblemSpec (spec) where

import Deslop.AST (EdgeKind (..), moduleNameUnsafe)
import Deslop.Problem (LintRuleId (LintRuleId), Location (..), Problem (..), ProblemId (..), ViolationKind (..), problemId)
import Deslop.Rule.Book (RuleId (RuleId), RulebookId (RulebookId))
import FileSystem.Path (encodeOsPath, relativePathUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe)

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
                        , autoFixable = False
                        }
            problemId p `shouldBe` ProblemId "no-relative-imports#src/Foo.ts"

        -- A Windows run decodes a native RelativePath with backslashes; the
        -- id has to stay spellable by (and matchable against) a baseline
        -- written on POSIX, so the separator is normalised.
        it "lint problem id normalises windows separators" $ do
            let p =
                    LintProblem
                        { lintRule = LintRuleId "no-relative-imports"
                        , location =
                            Location
                                { file = relativePathUnsafe (encodeOsPath "src\\features\\home\\home.ts")
                                , code = "import {x} from './x'"
                                }
                        , description = "No relative imports allowed"
                        , fix = "Use absolute imports"
                        , autoFixable = False
                        }
            problemId p `shouldBe` ProblemId "no-relative-imports#src/features/home/home.ts"

        it "rule violation id" $ do
            let p =
                    RuleViolation
                        { rulebook = RulebookId "architecture"
                        , rule = RuleId "no-barrel-imports"
                        , badModule = moduleNameUnsafe "@/lib/util"
                        , prose = "Barrel imports are forbidden"
                        , kind = DirectImport {imported = moduleNameUnsafe "@/lib/index", edge = ImportEdge, importStatement = "import { util } from '@/lib/index'"}
                        , fix = "Import directly from the module"
                        }
            problemId p `shouldBe` ProblemId "architecture#no-barrel-imports#@/lib/util"
