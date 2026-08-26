{- | Which keys a rulebook may contain, and what happens to one it may not.

The two halves are the same claim from either side. "Known keys" builds a rule
using every key the language has and asserts each one landed somewhere in the
DTO - a key permitted but never read would be a silent no-op, which is the bug
this module exists to prevent. "Unknown keys" then asserts that anything else is
a parse error naming the offender.

The unknown-key cases are the slips people actually make: a multi-word key in
camelCase, a clause key invented by analogy with a real one, a @transitive@ on
the clauses that have no such thing. Before this was an error every one of them
loaded cleanly and enforced less than the rulebook said, and a rule that quietly
does nothing is worse than no rule at all - nobody re-reads a passing run.
-}
module Deslop.Rule.Book.DtoSpec (spec) where

import Data.Text qualified as T
import Deslop.Rule.Book (RuleId (..))
import Deslop.Rule.Book.Dto (
    AllowsDto (..),
    ExistsDto (..),
    ForbidsDto (..),
    GlobDto (..),
    RuleDto (..),
    RulebookDto (..),
    UsesDto (..),
    parseRulebookYaml,
 )
import Fixtures.Deslop.Rule.Book.Dto (rulebookYaml)
import Hedgehog (Gen, annotate, assert, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Rule.Book.Dto" $ do
    knownKeysSpec
    unknownKeysSpec
    propertySpec

--------------------------------------------------------------------------------
-- Known keys
--------------------------------------------------------------------------------

knownKeysSpec :: Spec
knownKeysSpec = describe "known keys" $ do
    it "reads every key the rules language has" $
        parsedRule everyKey `shouldBe` Right everyKeyDto

    it "leaves the optional ones empty when they are not written" $
        parsedRule (rulebookYaml ["    target: \"@/x/**\""])
            `shouldBe` Right
                RuleDto
                    { id = RuleId "a-rule"
                    , description = "d"
                    , target = GlobDto "@/x/**"
                    , exclude = Nothing
                    , forbids = Nothing
                    , allows = Nothing
                    , allowsOnly = Nothing
                    , uses = Nothing
                    , exists = Nothing
                    , example = Nothing
                    , fix = "f"
                    }

-- | One rule wearing every clause and modifier at once.
everyKey :: ByteString
everyKey =
    rulebookYaml
        [ "    target: \"@/x/**\""
        , "    exclude:"
        , "      - \"@/x/**/*.spec\""
        , "    forbids:"
        , "      - import: \"@/y/**\""
        , "        transitive: true"
        , "    allows:"
        , "      - import: \"@/y/escape-hatch\""
        , "    allows-only:"
        , "      - import: \"@/z/**\""
        , "    uses:"
        , "      - import: \"@/w/thing\""
        , "        transitive: false"
        , "    exists:"
        , "      - module: \"{{TARGET_DIR}}/index\""
        , "    example: an example"
        ]

{- | Spelled out in full rather than checked field by field: every key of
'everyKey' has to show up on the right of this, so a permitted key the parser
forgets to read cannot pass.
-}
everyKeyDto :: RuleDto
everyKeyDto =
    RuleDto
        { id = RuleId "a-rule"
        , description = "d"
        , target = GlobDto "@/x/**"
        , exclude = Just [GlobDto "@/x/**/*.spec"]
        , forbids = Just [ForbidsImportDto {target = GlobDto "@/y/**", transitive = Just True}]
        , allows = Just [AllowsImportDto {target = GlobDto "@/y/escape-hatch"}]
        , allowsOnly = Just [AllowsImportDto {target = GlobDto "@/z/**"}]
        , uses = Just [UsesImportDto {target = GlobDto "@/w/thing", transitive = Just False}]
        , exists = Just [ExistsModuleDto {target = GlobDto "{{TARGET_DIR}}/index"}]
        , example = Just "an example"
        , fix = "f"
        }

--------------------------------------------------------------------------------
-- Unknown keys
--------------------------------------------------------------------------------

unknownKeysSpec :: Spec
unknownKeysSpec = describe "unknown keys" $ do
    it "rejects one in the rulebook envelope" $
        parseFailure "id: rb\nname: Rulebook\ndescription: d\nversion: 2\nrules: []\n"
            `shouldSatisfy` T.isInfixOf "version"

    it "rejects one in a rule, and says which" $ do
        let err = ruleFailure ["    target: \"@/x/**\"", "    uses-optional:", "      - \"@/y/**\""]
        err `shouldSatisfy` T.isInfixOf "unknown fields"
        err `shouldSatisfy` T.isInfixOf "uses-optional"

    it "rejects the camelCase spelling of a multi-word key" $
        ruleFailure ["    target: \"@/x/**\"", "    allowsOnly:", "      - import: \"@/y/**\""]
            `shouldSatisfy` T.isInfixOf "allowsOnly"

    it "rejects one in a forbids clause" $
        ruleFailure ["    target: \"@/x/**\"", "    forbids:", "      - import: \"@/y/**\"", "        transitve: true"]
            `shouldSatisfy` T.isInfixOf "transitve"

    it "rejects transitive on an allows clause, which does not take one" $
        ruleFailure ["    target: \"@/x/**\"", "    allows:", "      - import: \"@/y/**\"", "        transitive: true"]
            `shouldSatisfy` T.isInfixOf "transitive"

    it "rejects one in a uses clause" $
        ruleFailure ["    target: \"@/x/**\"", "    uses:", "      - import: \"@/y/**\"", "        optional: true"]
            `shouldSatisfy` T.isInfixOf "optional"

    it "rejects one in an exists clause" $
        ruleFailure ["    target: \"@/x/**\"", "    exists:", "      - module: \"@/y/index\"", "        transitive: true"]
            `shouldSatisfy` T.isInfixOf "transitive"

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

{- | The cases above are the keys people are known to have written. These are
the statement those cases are examples of: not "these seven keys are rejected"
but "everything outside the schema is", which is the only version of the claim
that covers the key nobody has thought of yet.

Each generated key is quoted in the YAML it goes into. Unquoted, a word like
@on@ or @null@ is a scalar YAML resolves to something that is not a string, and
the property would be about YAML's type resolution rather than about the schema.
The handwritten cases above are all unquoted, which is how an author writes them.
-}
propertySpec :: Spec
propertySpec = describe "properties" $ do
    prop "P1 a rule rejects every key its schema does not name" $ do
        key <- forAll (genKeyOtherThan ruleKeys)

        let err = ruleFailure ["    target: \"@/x/**\"", "    \"" <> key <> "\": v"]

        annotate (toString err)
        assert (key `T.isInfixOf` err)

    prop "P2 a clause rejects every key its schema does not name" $ do
        clause <- forAll (Gen.element clauses)
        key <- forAll (genKeyOtherThan clause.permitted)

        let err = ruleFailure (["    target: \"@/x/**\""] <> clauseLines clause key)

        annotate (toString err)
        assert (key `T.isInfixOf` err)

-- | Every key a rule may carry. Written out because that is what a schema is.
ruleKeys :: [Text]
ruleKeys =
    ["id", "description", "target", "exclude", "forbids", "allows", "allows-only", "uses", "exists", "example", "fix"]

{- | A clause as this spec needs to know it: what a rule calls it, the key
naming what it is about, and everything it will accept.
-}
data Clause = Clause
    { name :: Text
    , subject :: Text
    , permitted :: [Text]
    }
    deriving stock (Show)

-- | All four clause kinds, and the two rule keys that share 'AllowsDto'.
clauses :: [Clause]
clauses =
    [ Clause {name = "forbids", subject = "import", permitted = ["import", "transitive"]}
    , Clause {name = "allows", subject = "import", permitted = ["import"]}
    , Clause {name = "allows-only", subject = "import", permitted = ["import"]}
    , Clause {name = "uses", subject = "import", permitted = ["import", "transitive"]}
    , Clause {name = "exists", subject = "module", permitted = ["module"]}
    ]

clauseLines :: Clause -> Text -> [Text]
clauseLines clause key =
    [ "    " <> clause.name <> ":"
    , "      - " <> clause.subject <> ": \"@/y/**\""
    , "        \"" <> key <> "\": v"
    ]

-- | A kebab-case key, never one the caller already has a meaning for.
genKeyOtherThan :: [Text] -> Gen Text
genKeyOtherThan taken = Gen.filter (`notElem` taken) genKey
  where
    genKey = T.intercalate "-" <$> Gen.list (Range.linear 1 3) genWord
    genWord = Gen.text (Range.linear 1 6) Gen.lower

-- Helpers

-- | The single rule of a one-rule rulebook, or why the file did not parse.
parsedRule :: ByteString -> Either Text RuleDto
parsedRule bytes = do
    book <- parseRulebookYaml bytes
    maybeToRight "a rulebook with no rules" . viaNonEmpty head $ book.rules

parseFailure :: ByteString -> Text
parseFailure = either identity (const "<parsed successfully>") . parseRulebookYaml

-- | Why a one-rule rulebook with the given rule lines did not parse.
ruleFailure :: [Text] -> Text
ruleFailure = parseFailure . rulebookYaml
