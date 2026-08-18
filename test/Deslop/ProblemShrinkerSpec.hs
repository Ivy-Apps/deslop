module Deslop.ProblemShrinkerSpec (spec) where

import Deslop.Problem (LintRuleId (..), Location (..), Problem (..), ProblemId, ViolationKind (..), problemId)
import Deslop.ProblemShrinker (compactProblems)
import Deslop.Rulebook (RuleId (..), RulebookId (..))
import Effects.FileSystem (encodeOsPath, relativePathUnsafe)
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (prop)
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

{- | A transitive violation of @architecture#hooks-cant-use-components@ by
@bad@, arriving at the last module of @chain@.
-}
transitive :: Text -> [Text] -> Problem
transitive bad hops =
    RuleViolation
        { rulebook = RulebookId "architecture"
        , rule = RuleId "hooks-cant-use-components"
        , badModule = moduleIdUnsafe bad
        , prose = "Hooks may not reach components."
        , kind =
            TransitiveImport
                { chain = moduleIdUnsafe bad :| map moduleIdUnsafe hops
                , firstImport = ("import '" <>) <$> listToMaybe hops
                , alsoReached = []
                }
        , fix = "Remove the import."
        }

-- | The same Rule broken by @bad@ in a way that is not a transitive import.
missingUse :: Text -> Problem
missingUse bad =
    RuleViolation
        { rulebook = RulebookId "architecture"
        , rule = RuleId "hooks-cant-use-components"
        , badModule = moduleIdUnsafe bad
        , prose = "Hooks may not reach components."
        , kind = MissingUse {requiredImport = "@/hooks/base", transitive = False}
        , fix = "Import it."
        }

directImport :: Text -> Text -> Problem
directImport bad imported =
    RuleViolation
        { rulebook = RulebookId "architecture"
        , rule = RuleId "hooks-cant-use-components"
        , badModule = moduleIdUnsafe bad
        , prose = "Hooks may not reach components."
        , kind = DirectImport {imported = moduleIdUnsafe imported, importStatement = "import '" <> imported <> "'"}
        , fix = "Remove the import."
        }

lintProblem :: Text -> Problem
lintProblem file =
    LintProblem
        { lintRule = LintRuleId "no-relative-imports"
        , location = Location {file = relativePathUnsafe (encodeOsPath file), code = "import './x'"}
        , description = "Relative imports are not allowed."
        , fix = "Use an aliased import."
        , autoFixable = True
        }

absorbedBy :: Problem -> [[Text]]
absorbedBy RuleViolation {kind = TransitiveImport {alsoReached}} =
    map (map (.text) . toList) alsoReached
absorbedBy _ = []

chainOf :: Problem -> [Text]
chainOf RuleViolation {kind = TransitiveImport {chain}} = map (.text) (toList chain)
chainOf _ = []

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genProblem :: Gen Problem
genProblem =
    Gen.choice
        [ transitive <$> genModule <*> Gen.list (Range.linear 0 4) genModule
        , missingUse <$> genModule
        , directImport <$> genModule <*> genModule
        , lintProblem <$> Gen.element ["a.ts", "b.ts", "c.ts"]
        ]

-- | A handful of names, so that duplicates are common rather than incidental.
genModule :: Gen Text
genModule = Gen.element ["@/hooks/a", "@/hooks/b", "@/components/x", "@/components/y", "@/lib/z"]

genProblems :: Gen [Problem]
genProblems = Gen.list (Range.linear 0 30) genProblem

isTransitive :: Problem -> Bool
isTransitive RuleViolation {kind = TransitiveImport {}} = True
isTransitive _ = False

problemIds :: [Problem] -> [ProblemId]
problemIds = sort . map problemId

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Deslop.ProblemShrinker" $ do
    describe "compactProblems" $ do
        it "leaves a lone violation alone" $ do
            let only = transitive "@/hooks/a" ["@/components/x"]

            compactProblems [only] `shouldBe` [only]

        it "keeps the shortest chain when one import drags in a subtree" $ do
            -- The shape reported in #182: one import, five forbidden modules.
            let viaView hop = transitive "@/hooks/a" (["@/components/view"] <> hop)
                problems =
                    [ viaView []
                    , viaView ["@/components/banner"]
                    , viaView ["@/components/card"]
                    , viaView ["@/components/typography"]
                    , viaView ["@/components/utils"]
                    ]

            let compacted = compactProblems problems

            map chainOf compacted `shouldBe` [["@/hooks/a", "@/components/view"]]

        it "records every chain the survivor stands in for" $ do
            let viaView hop = transitive "@/hooks/a" (["@/components/view"] <> hop)

            let compacted = compactProblems [viaView [], viaView ["@/components/banner"], viaView ["@/components/card"]]

            map absorbedBy compacted
                `shouldBe` [
                             [ ["@/hooks/a", "@/components/view", "@/components/banner"]
                             , ["@/hooks/a", "@/components/view", "@/components/card"]
                             ]
                           ]

        it "compacts each violating module separately" $ do
            let problems =
                    [ transitive "@/hooks/a" ["@/components/view"]
                    , transitive "@/hooks/a" ["@/components/view", "@/components/card"]
                    , transitive "@/hooks/b" ["@/hooks/a", "@/components/view"]
                    , transitive "@/hooks/b" ["@/hooks/a", "@/components/view", "@/components/card"]
                    ]

            let compacted = compactProblems problems

            map chainOf compacted
                `shouldBe` [ ["@/hooks/a", "@/components/view"]
                           , ["@/hooks/b", "@/hooks/a", "@/components/view"]
                           ]

        it "breaks a tie on chain length by the chain itself" $ do
            let viaB = transitive "@/hooks/a" ["@/lib/b", "@/components/x"]
                viaC = transitive "@/hooks/a" ["@/lib/c", "@/components/x"]

            compactProblems [viaC, viaB] `shouldBe` compactProblems [viaB, viaC]

        it "does not merge a rule's other violations into its transitive one" $ do
            let problems = [transitive "@/hooks/a" ["@/components/x"], missingUse "@/hooks/a", directImport "@/hooks/a" "@/components/y"]

            length (compactProblems problems) `shouldBe` 3

        it "leaves direct import violations of one module apart" $ do
            let problems = [directImport "@/hooks/a" "@/components/x", directImport "@/hooks/a" "@/components/y"]

            compactProblems problems `shouldBe` sort problems

        it "leaves lint problems in the same file apart" $ do
            let problems = [lintProblem "a.ts", lintProblem "a.ts"]

            compactProblems problems `shouldBe` problems

    describe "properties" $ do
        prop "compacting is idempotent" $ do
            problems <- forAll genProblems

            compactProblems (compactProblems problems) === compactProblems problems

        prop "no ProblemId is lost, so a baseline suppresses exactly what it did before" $ do
            problems <- forAll genProblems

            ordNub (problemIds (compactProblems problems)) === ordNub (problemIds problems)

        prop "nothing but a transitive import is touched" $ do
            problems <- forAll genProblems

            sort (filter (not . isTransitive) (compactProblems problems))
                === sort (filter (not . isTransitive) problems)

        prop "the report never grows" $ do
            problems <- forAll genProblems

            assert (length (compactProblems problems) <= length problems)

        prop "the survivor's chain is the shortest of the ones it stands in for" $ do
            problems <- forAll genProblems

            let survivors = filter isTransitive (compactProblems problems)
                shortest p = all ((>= length (chainOf p)) . length) (absorbedBy p)
            filter (not . shortest) survivors === []

        prop "a survivor stands in for every duplicate that shared its ProblemId" $ do
            problems <- forAll genProblems

            let transitives = filter isTransitive problems
                survivors = filter isTransitive (compactProblems problems)
                absorbedCount = sum (map (length . absorbedBy) survivors)
            length survivors + absorbedCount === length transitives

        prop "the result is sorted, whatever order the rules were enforced in" $ do
            problems <- forAll genProblems

            let compacted = compactProblems problems
            compacted === sort compacted

        prop "the outcome does not depend on the order problems were reported in" $ do
            problems <- forAll genProblems

            compactProblems (reverse problems) === compactProblems problems
