{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}
module Deslop.RuleBookSpec (spec) where

import Control.Lens ((&), (.~), (?~))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Deslop.RuleBook
import Deslop.RuleBookFixtures qualified as Fix
import System.FilePath (takeBaseName, (</>))
import System.FilePath.Glob qualified as Glob
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (listFixtures)
import Text.Show.Pretty (ppShow)
import Utils (headOrThrow)

rbFixturesPath :: FilePath
rbFixturesPath = "test/fixtures/rulebook"

spec :: Spec
spec = do
    describe "parseRuleBookYaml" $
        runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ parseRuleBookTest

    describe "ruleBookFromDto" $ do
        it "preserves name" $ do
            let dto = Fix.defaultRuleBookDto & Fix.nameL .~ "MyBook"
            let rb = ruleBookFromDto dto
            rb.name `shouldBe` "MyBook"

        it "keeps rules that have a forbidden section" $ do
            let dto =
                    Fix.defaultRuleBookDto
                        & Fix.rulesL
                            .~ [ Fix.defaultRuleDto
                                    & Fix.forbiddenL
                                        ?~ [Fix.forbiddenImportDto "react" (Just False)]
                               ]
            length (ruleBookFromDto dto).rules `shouldBe` 1

        it "drop invalid rules" $ do
            let dto =
                    Fix.defaultRuleBookDto
                        & Fix.nameL .~ "Empty"
                        & Fix.rulesL .~ [Fix.defaultRuleDto & Fix.forbiddenL .~ Nothing]
            length (ruleBookFromDto dto).rules `shouldBe` 0

        it "compiles target globs" $ do
            let dto = Fix.defaultRuleBookDto
            let rule = headOrThrow (ruleBookFromDto dto).rules
            rule.target `shouldBe` (Glob.compile "*.ts" :| [])

        describe "Forbidden import" $ do
            it "defaults transitive to False when not specified" $ do
                let dto = Fix.defaultRuleBookDto
                let rule = headOrThrow (ruleBookFromDto dto).rules
                let forb = headOrThrow rule.forbidden
                forb.transitive `shouldBe` False

            it "preserves transitive False when specified" $ do
                let dto =
                        Fix.defaultRuleBookDto
                            & Fix.rulesL
                                .~ [ Fix.defaultRuleDto
                                        & Fix.forbiddenL
                                            ?~ [Fix.forbiddenImportDto "lib" (Just False)]
                                   ]
                let rule = headOrThrow (ruleBookFromDto dto).rules
                let forb = headOrThrow rule.forbidden
                forb.transitive `shouldBe` False

            it "preserves transitive True when specified" $ do
                let dto =
                        Fix.defaultRuleBookDto
                            & Fix.rulesL
                                .~ [ Fix.defaultRuleDto
                                        & Fix.forbiddenL
                                            ?~ [Fix.forbiddenImportDto "lib" (Just True)]
                                   ]
                let rule = headOrThrow (ruleBookFromDto dto).rules
                let forb = headOrThrow rule.forbidden
                forb.transitive `shouldBe` True

    describe "RuleBook Monoid" $ do
        it "left identity" $ do
            let x =  ruleBookFromDto Fix.defaultRuleBookDto
            let res = mempty <> x
            res `shouldBe` x
            res.name `shouldBe` "Test"

        it "right identity" $ do
            let x =  ruleBookFromDto Fix.defaultRuleBookDto
            let res = mempty <> x
            res `shouldBe` x
            res.name `shouldBe` "Test"

        it "associativity" $ do
            let a = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "A")
            let b = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "B")
            let c = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "C")
            ((a <> b) <> c) `shouldBe` (a <> (b <> c))

        it "(<>) combines rulebooks" $ do
            let rb1 = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "Second")
            let rb2 = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "First")
            let combined = rb1 <> rb2
            combined.name `shouldBe` "First <> Second"
            length combined.rules `shouldBe` 2
            (headOrThrow combined.rules).id `shouldBe` (headOrThrow rb1.rules).id
            (combined.rules !! 1).id `shouldBe` (headOrThrow rb2.rules).id
  where
    parseRuleBookTest :: FilePath -> Spec
    parseRuleBookTest fpath = do
        let testName = takeBaseName fpath
        it ("case:" <> testName) $ do
            ruleBookYaml <- TE.encodeUtf8 <$> TIO.readFile (rbFixturesPath </> fpath)
            let ruleBookRes = parseRuleBookYaml ruleBookYaml
            return $ defaultGolden testName (ppShow ruleBookRes)
