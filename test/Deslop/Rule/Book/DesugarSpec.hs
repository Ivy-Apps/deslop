{- | What @allows-only@ means, and what desugaring may not do while it means it.

The properties are the interesting half. D1 is a differential test with no model
to write: @allows-only@ claims to be shorthand for a rulebook the author could
have typed out, so the oracle /is/ that longhand, and the property is that the
two desugar to the same thing. D2 and D3 pin the two invariants the whole
surface-versus-core split rests on - that the pass cannot fail, and that it
touches nothing it was not asked to.

Globs are generated as arbitrary text on purpose. The desugarer must never look
inside one; if it ever starts to, these properties are what notices.
-}
module Deslop.Rule.Book.DesugarSpec (spec) where

import Deslop.Rule.Book (RuleId (..))
import Deslop.Rule.Book.Desugar (DesugaredRuleDto (..), desugarRule, desugarRulebook)
import Deslop.Rule.Book.Dto (
    AllowsDto (..),
    ExistsDto (..),
    ForbidsDto (..),
    GlobDto (..),
    RuleDto (..),
    RulebookDto (..),
    UsesDto (..),
 )

-- Both records share most of their field names, so a record update has to say
-- which one it means. Reading a field does not: the type of the value settles it.
import Deslop.Rule.Book.Dto qualified as Dto
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Rule.Book.Desugar" $ do
    allowsOnlySpec
    untouchedSpec
    propertySpec

--------------------------------------------------------------------------------
-- allows-only
--------------------------------------------------------------------------------

allowsOnlySpec :: Spec
allowsOnlySpec = describe "allows-only" $ do
    it "forbids everything and allows back what was listed" $ do
        let desugared = desugarRule bareRule {Dto.allowsOnly = Just [allowsOf "@/shared/**"]}

        desugared.forbids `shouldBe` Just [forbidsOf "**"]
        desugared.allows `shouldBe` Just [allowsOf "@/shared/**"]

    it "forbids everything exactly once, however many imports were listed" $ do
        let listed = allowsOf <$> ["@/shared/**", "@/lib/**", "@/config"]
            desugared = desugarRule bareRule {Dto.allowsOnly = Just listed}

        desugared.forbids `shouldBe` Just [forbidsOf "**"]
        desugared.allows `shouldBe` Just listed

    it "keeps a hand-written forbids, which may say things the generated one cannot" $ do
        let transitive = ForbidsImportDto {target = GlobDto "@/server/**", transitive = Just True}
            desugared =
                desugarRule
                    bareRule
                        { Dto.forbids = Just [transitive]
                        , Dto.allowsOnly = Just [allowsOf "@/shared/**"]
                        }

        desugared.forbids `shouldBe` Just [transitive, forbidsOf "**"]

    it "merges into a hand-written allows rather than replacing it" $ do
        let desugared =
                desugarRule
                    bareRule
                        { Dto.allows = Just [allowsOf "@/lib/**"]
                        , Dto.allowsOnly = Just [allowsOf "@/shared/**"]
                        }

        desugared.allows `shouldBe` Just [allowsOf "@/lib/**", allowsOf "@/shared/**"]

    {- An empty list is not the same as no list: it says "these and nothing
    else" of nothing at all, so everything stays forbidden. -}
    it "forbids everything and allows nothing back when the list is empty" $ do
        let desugared = desugarRule bareRule {Dto.allowsOnly = Just []}

        desugared.forbids `shouldBe` Just [forbidsOf "**"]
        desugared.allows `shouldBe` Just []

    it "adds no forbids at all when the rule has no allows-only" $ do
        let desugared = desugarRule bareRule {Dto.allows = Just [allowsOf "@/lib/**"]}

        desugared.forbids `shouldBe` Nothing
        desugared.allows `shouldBe` Just [allowsOf "@/lib/**"]

--------------------------------------------------------------------------------
-- Everything else
--------------------------------------------------------------------------------

untouchedSpec :: Spec
untouchedSpec = describe "the rest of a rulebook" $
    it "keeps its rules in the order they were written" $ do
        let identifiers = ["a", "b", "c"]
            book = bareBook {rules = ruleNamed <$> identifiers}

        fmap (.id) (desugarRulebook book).rules `shouldBe` (RuleId <$> identifiers)

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

propertySpec :: Spec
propertySpec = describe "properties" $ do
    prop "D1 allows-only desugars to exactly what its longhand desugars to" $ do
        rule <- forAll genRuleDto
        only <- forAll (Gen.list (Range.linear 0 3) genAllows)

        let sugared = rule {Dto.allowsOnly = Just only}
            longhand =
                rule
                    { Dto.allowsOnly = Nothing
                    , Dto.forbids = Just (fromMaybe [] rule.forbids <> [forbidsOf "**"])
                    , Dto.allows = Just (fromMaybe [] rule.allows <> only)
                    }

        desugarRule sugared === desugarRule longhand

    prop "D2 desugaring is total, whatever a rule says" $ do
        rule <- forAll genRuleDto
        let desugared = desugarRule rule
        {- Comparing the result with itself forces every field of it. The
        property is that doing so terminates and yields an answer, whatever was
        written - a pass that can fail is not sugar. -}
        assert (desugared == desugared)

    prop "D3 desugaring touches nothing but forbids and allows" $ do
        rule <- forAll genRuleDto
        let desugared = desugarRule rule

        (desugared.id, desugared.target, desugared.exclude) === (rule.id, rule.target, rule.exclude)
        (desugared.uses, desugared.exists) === (rule.uses, rule.exists)
        (desugared.description, desugared.example, desugared.fix)
            === (rule.description, rule.example, rule.fix)

--------------------------------------------------------------------------------
-- Fixtures and generators
--------------------------------------------------------------------------------

bareBook :: RulebookDto RuleDto
bareBook =
    RulebookDto
        { id = "test-rulebook"
        , name = "Test rulebook"
        , description = "Rulebook used for testing"
        , rules = []
        }

bareRule :: RuleDto
bareRule = ruleNamed "test-rule"

{- | A rule with no clauses at all. Spelled out rather than updated from
'bareRule': most of these field names belong to more than one record in scope,
and a record /update/ then has no unambiguous way to say which is meant.
-}
ruleNamed :: Text -> RuleDto
ruleNamed identifier =
    RuleDto
        { id = RuleId identifier
        , description = "test"
        , target = GlobDto "@/**"
        , exclude = Nothing
        , forbids = Nothing
        , allows = Nothing
        , allowsOnly = Nothing
        , uses = Nothing
        , exists = Nothing
        , example = Nothing
        , fix = ""
        }

forbidsOf :: Text -> ForbidsDto
forbidsOf glob = ForbidsImportDto {target = GlobDto glob, transitive = Nothing}

allowsOf :: Text -> AllowsDto
allowsOf = AllowsImportDto . GlobDto

{- | Globs are arbitrary text, not patterns. Desugaring runs before anything
parses one, so a generator that only produced valid Glob+ would be testing less
than the code promises.
-}
genGlob :: Gen GlobDto
genGlob = GlobDto <$> Gen.text (Range.linear 0 12) Gen.unicode

genAllows :: Gen AllowsDto
genAllows = AllowsImportDto <$> genGlob

genRuleDto :: Gen RuleDto
genRuleDto = do
    forbids <- optional' (ForbidsImportDto <$> genGlob <*> Gen.maybe Gen.bool)
    allows <- optional' genAllows
    allowsOnly <- optional' genAllows
    uses <- optional' (UsesImportDto <$> genGlob <*> Gen.maybe Gen.bool)
    exists <- optional' (ExistsModuleDto <$> genGlob)
    exclude <- optional' genGlob
    identifier <- Gen.text (Range.linear 1 8) Gen.alphaNum
    prose <- Gen.text (Range.linear 0 16) Gen.unicode
    illustration <- Gen.maybe (Gen.text (Range.linear 0 8) Gen.unicode)
    target <- genGlob
    pure
        RuleDto
            { id = RuleId identifier
            , description = prose
            , target = target
            , exclude = exclude
            , forbids = forbids
            , allows = allows
            , allowsOnly = allowsOnly
            , uses = uses
            , exists = exists
            , example = illustration
            , fix = prose
            }
  where
    optional' gen = Gen.maybe (Gen.list (Range.linear 0 3) gen)
