module Deslop.RuleBookSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Deslop.RuleBook
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
            let dto = minimalRuleBookDto "MyBook"
            let rb = ruleBookFromDto dto
            rb.name `shouldBe` "MyBook"

        it "keeps rules that have a forbidden section" $ do
            let dto = ruleBookDtoWithOneRule "r1" (Just [forbiddenImportDto "react" (Just False)])
            length (ruleBookFromDto dto).rules `shouldBe` 1

        it "drop invalid rules" $ do
            let dto = RuleBookDto "Empty" [emptyRuleDto "r1"]
            length (ruleBookFromDto dto).rules `shouldBe` 0

        it "compiles target globs" $ do
            let dto = ruleBookDtoWithOneRule "r1" (Just [forbiddenImportDto "react" Nothing])
            let rule = headOrThrow (ruleBookFromDto dto).rules
            rule.target `shouldBe` (Glob.compile "*.ts" :| [])

        describe "Forbidden import" $ do
            it "defaults transitive to False when not specified" $ do
                let dto = ruleBookDtoWithOneRule "r1" (Just [forbiddenImportDto "react" Nothing])
                let rule = headOrThrow (ruleBookFromDto dto).rules
                let forb = headOrThrow rule.forbidden
                forb.transitive `shouldBe` False

            it "preserves transitive False when specified" $ do
                let dto = ruleBookDtoWithOneRule "r1" (Just [forbiddenImportDto "lib" (Just False)])
                let rule = headOrThrow (ruleBookFromDto dto).rules
                let forb = headOrThrow rule.forbidden
                forb.transitive `shouldBe` False

            it "preserves transitive True when specified" $ do
                let dto = ruleBookDtoWithOneRule "r1" (Just [forbiddenImportDto "lib" (Just True)])
                let rule = headOrThrow (ruleBookFromDto dto).rules
                let forb = headOrThrow rule.forbidden
                forb.transitive `shouldBe` True
  where
    parseRuleBookTest :: FilePath -> Spec
    parseRuleBookTest fpath = do
        let testName = takeBaseName fpath
        it ("case:" <> testName) $ do
            ruleBookYaml <- TE.encodeUtf8 <$> TIO.readFile (rbFixturesPath </> fpath)
            let ruleBookRes = parseRuleBookYaml ruleBookYaml
            return $ defaultGolden testName (ppShow ruleBookRes)

minimalRuleBookDto :: Text -> RuleBookDto
minimalRuleBookDto name =
    RuleBookDto name [ruleDtoWithForbidden "r1" (Just [forbiddenImportDto "react" Nothing])]

ruleBookDtoWithOneRule :: Text -> Maybe [ForbiddenDto] -> RuleBookDto
ruleBookDtoWithOneRule ruleId forb =
    RuleBookDto "Test" [ruleDtoWithForbidden ruleId forb]

ruleDtoWithForbidden :: Text -> Maybe [ForbiddenDto] -> RuleDto
ruleDtoWithForbidden rId = RuleDto (RuleId rId) Nothing (GlobDto "*.ts" :| []) Nothing

emptyRuleDto :: Text -> RuleDto
emptyRuleDto rId =
    RuleDto (RuleId rId) Nothing (GlobDto "*.ts" :| []) Nothing Nothing

forbiddenImportDto :: String -> Maybe Bool -> ForbiddenDto
forbiddenImportDto glob = ForbiddenImportDto (GlobDto glob)
