module Deslop.Problem.FormatterSpec (spec) where

import Deslop.Module (EdgeKind (..), Location (..), ModuleName (..), moduleNameUnsafe)
import Deslop.Problem (LintRuleId (LintRuleId), Problem (..), ViolationKind (..))
import Deslop.Problem.Formatter (formatProblem)
import Deslop.Rule.Book (RuleId (RuleId), RulebookId (RulebookId))
import FileSystem.Path (encodeOsPath, relativePathUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe)

lintProblem :: Problem
lintProblem =
    LintProblem
        { lintRule = LintRuleId "no-relative-imports"
        , location =
            Location
                { file = relativePathUnsafe (encodeOsPath "src/Foo.ts")
                , line = 1
                , code = "   import {bar} from './bar'   \n"
                }
        , description = "No relative imports allowed"
        , fix = "Use absolute imports"
        , autoFixable = False
        }

ruleViolation :: Problem
ruleViolation = violationOf DirectImport {edge = ImportEdge, imported = moduleNameUnsafe "@/lib/index", location = statementAt 3 "import { x } from '@/lib/index'"}

violationOf :: ViolationKind -> Problem
violationOf violationKind =
    RuleViolation
        { rulebook = RulebookId "architecture"
        , rule = RuleId "no-barrel-imports"
        , badModule = moduleNameUnsafe "@/lib/util"
        , modulePath = relativePathUnsafe (encodeOsPath "src/lib/util.ts")
        , prose = "Barrel imports are forbidden"
        , kind = violationKind
        , fix = "Import directly from the module"
        }

-- | A statement written at a given line of the module every violation here is
-- reported against.
statementAt :: Int -> Text -> Location
statementAt line code =
    Location
        { file = relativePathUnsafe (encodeOsPath "src/lib/util.ts")
        , line = line
        , code = code
        }

-- | @@/lib/util -> @/lib/a -> @/forbids/store@, the chain most cases start from.
twoHopChain :: NonEmpty ModuleName
twoHopChain = moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/lib/a", moduleNameUnsafe "@/forbids/store"]

transitiveVia :: Text -> ModuleName -> [NonEmpty ModuleName] -> Problem
transitiveVia hop forbidden absorbed =
    violationOf
        TransitiveImport
            { chain = moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe hop, forbidden]
            , firstImport = Just . statementAt 7 $ "import { x } from '" <> hop <> "'"
            , alsoReached = absorbed
            }

spec :: Spec
spec = describe "Deslop.Problem.Formatter" $ do
    describe "formatProblem" $ do
        describe "LintProblem" $ do
            it "formats header with problem id" $ do
                let result = formatProblem lintProblem
                result
                    `shouldBe` "# no-relative-imports#src/Foo.ts\n"
                        <> "src/Foo.ts:1\n"
                        <> "No relative imports allowed\n"
                        <> "```ts\nimport {bar} from './bar'\n```\n"
                        <> "FIX: Use absolute imports"

            it "strips whitespace from fix" $ do
                let p = lintProblem {fix = "  Use absolute imports  "}
                let result = formatProblem p
                result
                    `shouldBe` "# no-relative-imports#src/Foo.ts\n"
                        <> "src/Foo.ts:1\n"
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
                                    , line = 1
                                    , code = "   import {bar} from './bar'   \n"
                                    }
                            , description = "No relative imports allowed"
                            , fix = "Use absolute imports"
                            , autoFixable = True
                            }
                let result = formatProblem p
                result
                    `shouldBe` "[AUTO-FIXABLE] # no-relative-imports#src/Foo.ts\n"
                        <> "src/Foo.ts:1\n"
                        <> "No relative imports allowed\n"
                        <> "```ts\nimport {bar} from './bar'\n```\n"
                        <> "FIX: Use absolute imports"

        describe "RuleViolation" $ do
            it "formats header with problem id" $ do
                let result = formatProblem ruleViolation
                result
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts:3\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' directly imports '@/lib/index'.\n"
                        <> "```ts\nimport { x } from '@/lib/index'\n```\n"
                        <> "FIX: Import directly from the module"

            it "strips whitespace from fix" $ do
                let p = ruleViolation {fix = "  Import directly from the module  "}
                let result = formatProblem p
                result
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts:3\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' directly imports '@/lib/index'.\n"
                        <> "```ts\nimport { x } from '@/lib/index'\n```\n"
                        <> "FIX: Import directly from the module"

            it "spells out the chain of a transitive import" $ do
                let p = violationOf TransitiveImport {chain = twoHopChain, firstImport = Just (statementAt 7 "import { x } from '@/lib/a'"), alsoReached = []}

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts:7\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "FIX: Import directly from the module"

            it "counts a single hop in the singular and omits the code block when the chain has no first hop" $ do
                let p =
                        violationOf
                            TransitiveImport
                                { chain = moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/forbids/store"]
                                , firstImport = Nothing
                                , alsoReached = []
                                }

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (1 hop) via: "
                        <> "@/lib/util → @/forbids/store.\n"
                        <> "FIX: Import directly from the module"

            it "attributes absorbed duplicates to this import when they share its first hop" $ do
                let absorbed =
                        [ moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/lib/a", moduleNameUnsafe "@/forbids/other"]
                        , moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/lib/a", moduleNameUnsafe "@/forbids/third"]
                        ]
                    p = transitiveVia "@/lib/a" (moduleNameUnsafe "@/forbids/store") absorbed

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts:7\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "Also reaches 2 more forbidden modules through this import.\n"
                        <> "FIX: Import directly from the module"

            it "names the other imports at fault when absorbed duplicates come through them" $ do
                let absorbed =
                        [ moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/lib/b", moduleNameUnsafe "@/forbids/other"]
                        , moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/lib/c", moduleNameUnsafe "@/forbids/third"]
                        ]
                    p = transitiveVia "@/lib/a" (moduleNameUnsafe "@/forbids/store") absorbed

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts:7\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' transitively imports '@/forbids/store' (2 hops) via: "
                        <> "@/lib/util → @/lib/a → @/forbids/store.\n"
                        <> "```ts\nimport { x } from '@/lib/a'\n```\n"
                        <> "Also reaches 2 more forbidden modules, through the imports of '@/lib/b', '@/lib/c'.\n"
                        <> "FIX: Import directly from the module"

            it "puts a lone absorbed duplicate in the singular" $ do
                let absorbed = [moduleNameUnsafe "@/lib/util" :| [moduleNameUnsafe "@/lib/b", moduleNameUnsafe "@/forbids/other"]]
                    p = transitiveVia "@/lib/a" (moduleNameUnsafe "@/forbids/store") absorbed

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts:7\n"
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
                                    <> "src/lib/util.ts\n"
                                    <> "Barrel imports are forbidden\n\n"
                                    <> "Module '@/lib/util' must import '@/lib/logger'.\n"
                                    <> "FIX: Import directly from the module"
                               , "# architecture#no-barrel-imports#@/lib/util\n"
                                    <> "src/lib/util.ts\n"
                                    <> "Barrel imports are forbidden\n\n"
                                    <> "Module '@/lib/util' must transitively import '@/lib/logger'.\n"
                                    <> "FIX: Import directly from the module"
                               )

            it "names the module a rule requires to exist" $ do
                let p = violationOf MissingModule {requiredModule = moduleNameUnsafe "@/lib/util.spec"}

                formatProblem p
                    `shouldBe` "# architecture#no-barrel-imports#@/lib/util\n"
                        <> "src/lib/util.ts\n"
                        <> "Barrel imports are forbidden\n\n"
                        <> "Module '@/lib/util' requires '@/lib/util.spec' to exist.\n"
                        <> "FIX: Import directly from the module"
