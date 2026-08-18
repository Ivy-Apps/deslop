module Deslop.ProblemFormatterSpec (spec) where

import Deslop.Problem (LintRuleId (LintRuleId), Location (..), Problem (..), ViolationKind (..))
import Deslop.ProblemFormatter (formatProblem)
import Deslop.Rulebook (RuleId (RuleId), RulebookId (RulebookId))
import Effects.FileSystem (encodeOsPath, relativePathUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)

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
ruleViolation = violationOf DirectImport {imported = moduleIdUnsafe "@/lib/index", importStatement = "import { x } from '@/lib/index'"}

violationOf :: ViolationKind -> Problem
violationOf violationKind =
    RuleViolation
        { rulebook = RulebookId "architecture"
        , rule = RuleId "no-barrel-imports"
        , badModule = moduleIdUnsafe "@/lib/util"
        , prose = "Barrel imports are forbidden"
        , kind = violationKind
        , fix = "Import directly from the module"
        }

-- | @@/lib/util -> @/lib/a -> @/forbids/store@, the chain most cases start from.
twoHopChain :: NonEmpty ModuleId
twoHopChain = moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/lib/a", moduleIdUnsafe "@/forbids/store"]

transitiveVia :: Text -> ModuleId -> [NonEmpty ModuleId] -> Problem
transitiveVia hop forbidden absorbed =
    violationOf
        TransitiveImport
            { chain = moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe hop, forbidden]
            , firstImport = Just $ "import { x } from '" <> hop <> "'"
            , alsoReached = absorbed
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
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' directly imports '@/lib/index'.\n"
                        <> "```ts\nimport { x } from '@/lib/index'\n```\n"
                        <> "FIX: Import directly from the module"

            it "strips whitespace from fix" $ do
                let p = ruleViolation {fix = "  Import directly from the module  "}
                let result = formatProblem p
                result
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' directly imports '@/lib/index'.\n"
                        <> "```ts\nimport { x } from '@/lib/index'\n```\n"
                        <> "FIX: Import directly from the module"

            it "spells out the chain of a transitive import" $ do
                let p = violationOf TransitiveImport {chain = twoHopChain, firstImport = Just "import { x } from '@/lib/a'", alsoReached = []}

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "FIX: Import directly from the module"

            it "counts a single hop in the singular and omits the code block when the chain has no first hop" $ do
                let p =
                        violationOf
                            TransitiveImport
                                { chain = moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/forbids/store"]
                                , firstImport = Nothing
                                , alsoReached = []
                                }

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (1 hop) via: "
                        <> "@/lib/util → @/forbids/store.\n"
                        <> "FIX: Import directly from the module"

            it "attributes absorbed duplicates to this import when they share its first hop" $ do
                let absorbed =
                        [ moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/lib/a", moduleIdUnsafe "@/forbids/other"]
                        , moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/lib/a", moduleIdUnsafe "@/forbids/third"]
                        ]
                    p = transitiveVia "@/lib/a" (moduleIdUnsafe "@/forbids/store") absorbed

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "Also reaches 2 more forbidden modules through this import.\n"
                        <> "FIX: Import directly from the module"

            it "names the other imports at fault when absorbed duplicates come through them" $ do
                let absorbed =
                        [ moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/lib/b", moduleIdUnsafe "@/forbids/other"]
                        , moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/lib/c", moduleIdUnsafe "@/forbids/third"]
                        ]
                    p = transitiveVia "@/lib/a" (moduleIdUnsafe "@/forbids/store") absorbed

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "Also reaches 2 more forbidden modules, through the imports of '@/lib/b', '@/lib/c'.\n"
                        <> "FIX: Import directly from the module"

            it "puts a lone absorbed duplicate in the singular" $ do
                let absorbed = [moduleIdUnsafe "@/lib/util" :| [moduleIdUnsafe "@/lib/b", moduleIdUnsafe "@/forbids/other"]]
                    p = transitiveVia "@/lib/a" (moduleIdUnsafe "@/forbids/store") absorbed

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "Also reaches 1 more forbidden module, through the import of '@/lib/b'.\n"
                        <> "FIX: Import directly from the module"

            it "distinguishes a required import from a required transitive one" $ do
                let direct = violationOf MissingUse {requiredImport = "@/lib/logger", transitive = False}
                    transitive = violationOf MissingUse {requiredImport = "@/lib/logger", transitive = True}

                (formatProblem direct, formatProblem transitive)
                    `shouldBe` ( "# architecture#no-barrel-imports#@/lib/util\n"
                                    <> "Barrel imports are forbidden\n\n"
                                    <> "Module '@/lib/util' must import '@/lib/logger'.\n"
                                    <> "FIX: Import directly from the module"
                               , "# architecture#no-barrel-imports#@/lib/util\n"
                                    <> "Barrel imports are forbidden\n\n"
                                    <> "Module '@/lib/util' must transitively import '@/lib/logger'.\n"
                                    <> "FIX: Import directly from the module"
                               )

            it "names the module a rule requires to exist" $ do
                let p = violationOf MissingModule {requiredModule = moduleIdUnsafe "@/lib/util.spec"}

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' requires '@/lib/util.spec' to exist.\n"
                        <> "FIX: Import directly from the module"
