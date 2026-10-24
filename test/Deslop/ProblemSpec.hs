module Deslop.ProblemSpec (spec) where

import Deslop.Module (EdgeKind (..), moduleNameUnsafe)
import Deslop.Problem (ProblemId (..), ViolationKind (..), problemId)
import Fixtures.Deslop.Problem (brokenRule, lintProblemAt, locatedAt, violationOf)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Deslop.Problem" $ do
    describe "problemId" $ do
        it "lint problem id" $ do
            let p = lintProblemAt "no-relative-imports" (locatedAt "src/Foo.ts" 1 "import {bar} from './bar'")

            problemId p `shouldBe` ProblemId "no-relative-imports#src/Foo.ts"

        -- A Windows run decodes a native ProjectRelativePath with backslashes; the
        -- id has to stay spellable by (and matchable against) a baseline
        -- written on POSIX, so the separator is normalised.
        it "lint problem id normalises windows separators" $ do
            let p = lintProblemAt "no-relative-imports" (locatedAt "src\\features\\home\\home.ts" 1 "import {x} from './x'")

            problemId p `shouldBe` ProblemId "no-relative-imports#src/features/home/home.ts"

        it "rule violation id" $ do
            let p =
                    violationOf
                        (brokenRule "architecture" "no-barrel-imports")
                        "@/lib/util"
                        DirectImport
                            { imported = moduleNameUnsafe "@/lib/index"
                            , edge = ImportEdge
                            , location = locatedAt "src/lib/util.ts" 4 "import { util } from '@/lib/index'"
                            }

            problemId p `shouldBe` ProblemId "architecture#no-barrel-imports#@/lib/util"
