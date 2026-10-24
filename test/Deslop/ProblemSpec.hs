module Deslop.ProblemSpec (spec) where

import Deslop.Module (EdgeKind (..), moduleNameUnsafe, specifierUnsafe)
import Deslop.Problem (ProblemId (..), ViolationKind (..), problemId)
import Fixtures.Deslop.Problem (brokenRule, lintProblemAt, locatedAt, unjudgedImportOf, violationOf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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

        describe "of a dependency that cannot be judged" $ do
            let layering = brokenRule "architecture" "layering"
                unjudged = unjudgedImportOf layering

            it "names the module that wrote the dependency, then the dependency" $
                problemId (unjudged ("app/main" :| ["lib/relay"]) "./. + n")
                    `shouldBe` ProblemId "architecture#layering#lib/relay#unresolved#./. + n"

            -- Adding or removing a target must not unsuppress an accepted one.
            it "is the same whichever target reaches the dependency, and however" $
                problemId (unjudged ("app/other" :| ["app/main", "lib/relay"]) "./. + n")
                    `shouldBe` problemId (unjudged ("app/main" :| ["lib/relay"]) "./. + n")

            -- Baselining one dynamic import must not accept the next one, nor
            -- a violation of the same Rule that Deslop could judge.
            it "differs from the module's other violations of the Rule, and between dependencies" $ do
                let judged = violationOf layering "app/main" MissingUse {requiredImport = "lib/**", transitive = False}
                    ids = problemId <$> [judged, unjudged ("app/main" :| []) "./gone.nix", unjudged ("app/main" :| []) "./. + n"]

                ids `shouldSatisfy` ((== ids) . ordNub)

            it "does not move with the line the dependency is written on" $ do
                let movedDown =
                        violationOf
                            layering
                            "app/main"
                            UnjudgedImport
                                { chain = one (moduleNameUnsafe "app/main")
                                , specifier = specifierUnsafe "./gone.nix"
                                , edge = ImportEdge
                                , location = locatedAt "app/main.ts" 40 "import (./gone.nix)"
                                }

                problemId movedDown `shouldBe` problemId (unjudged ("app/main" :| []) "./gone.nix")
