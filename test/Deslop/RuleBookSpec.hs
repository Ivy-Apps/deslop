{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}
module Deslop.RuleBookSpec (spec) where

import Control.Lens ((.~), (?~))
import Data.List ((!!))
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Deslop.RuleBook
import Deslop.RuleBookFixtures qualified as Fix
import Effectful (runEff)
import Effects.FileSystem (runFileSystemIO)
import FsEncoding (encodePathString)
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
    describe "ruleBookFromFile" $
        runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ ruleBookFromFileTest
    describe "loadRuleBook" $ do
        it "valid-rules-1" $ do
            res <- runEff . runFileSystemIO $ loadRuleBookFrom (encodePathString (rbFixturesPath </> "valid-rules-1"))
            return $ defaultGolden "loadRuleBook--valid-rules-1" (ppShow res)

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
                        & Fix.nameL
                        .~ "Empty"
                            & Fix.rulesL
                        .~ [Fix.defaultRuleDto & Fix.forbiddenL .~ Nothing]
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
            let x = ruleBookFromDto Fix.defaultRuleBookDto
            let res = mempty <> x
            res `shouldBe` x
            res.name `shouldBe` "Test"

        it "right identity" $ do
            let x = ruleBookFromDto Fix.defaultRuleBookDto
            let res = mempty <> x
            res `shouldBe` x
            res.name `shouldBe` "Test"

        it "associativity" $ do
            let a = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "A")
            let b = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "B")
            let c = ruleBookFromDto (Fix.defaultRuleBookDto & Fix.nameL .~ "C")
            ((a <> b) <> c) `shouldBe` (a <> (b <> c))

        it "(<>) combines rulebooks" $ do
            -- Given
            let ruleOne = Fix.defaultRuleDto & idL .~ RuleId "rule-one"
            let ruleTwo = Fix.defaultRuleDto & idL .~ RuleId "rule-two"
            let rb1 =
                    ruleBookFromDto
                        ( Fix.defaultRuleBookDto
                            & Fix.nameL
                            .~ "First"
                                & Fix.rulesL
                            .~ [ruleOne]
                        )
            let rb2 =
                    ruleBookFromDto
                        ( Fix.defaultRuleBookDto
                            & Fix.nameL
                            .~ "Second"
                                & Fix.rulesL
                            .~ [ruleTwo]
                        )

            -- When
            let combined = rb1 <> rb2

            -- Then
            combined.name `shouldBe` "First <> Second"
            length combined.rules `shouldBe` 2
            (headOrThrow combined.rules).id `shouldBe` RuleId "rule-one"
            (combined.rules !! 1).id `shouldBe` RuleId "rule-two"
  where
    parseRuleBookTest :: FilePath -> Spec
    parseRuleBookTest fpath = do
        let testName = "rulebook-dto-from-yaml--" <> takeBaseName fpath
        it ("case: " <> testName) $ do
            ruleBookYaml <- TE.encodeUtf8 <$> TIO.readFile (rbFixturesPath </> fpath)
            let ruleBookRes = parseRuleBookYaml ruleBookYaml
            return $ defaultGolden testName (ppShow ruleBookRes)

    ruleBookFromFileTest :: FilePath -> Spec
    ruleBookFromFileTest fpath = do
        let testName = "rulebook-from-file--" <> takeBaseName fpath
        it ("case: " <> testName) $ do
            res <- runEff . runFileSystemIO $ ruleBookFromFile (encodePathString (rbFixturesPath </> fpath))
            return $ defaultGolden testName (ppShow res)
