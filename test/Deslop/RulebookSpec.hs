module Deslop.RulebookSpec (spec) where

import Data.Text qualified as T
import Deslop.Rulebook
import Effectful (runEff)
import Effects.FileSystem (decodeOsPath, runFileSystemIO)
import System.OsPath (OsPath, osp, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (listFixtures, mkAbsolute)
import Text.Show.Pretty (ppShow)

rbFixturesPath :: OsPath
rbFixturesPath = [osp|test/fixtures/rulebook|]

spec :: Spec
spec = describe "Deslop.Rulebook" $ do
    describe "rulebookFromFile" $
        runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ ruleBookFromFileTest
    shippedExamplesSpec
    globCompilationSpec
  where
    ruleBookFromFileTest :: OsPath -> Spec
    ruleBookFromFileTest fpath = do
        let testName = T.unpack $ "rulebook-from-file--" <> decodeOsPath (takeBaseName fpath)
        it ("case: " <> testName) $ do
            rbPath <- mkAbsolute (rbFixturesPath </> fpath)
            res <- runEff . runFileSystemIO $ ruleBookFromFile rbPath
            return $ defaultGolden testName (ppShow res)

--------------------------------------------------------------------------------
-- Shipped example rulebooks
--------------------------------------------------------------------------------

{- | The README advertises these as rulebooks to copy into a project, so one
that fails to compile aborts the run of everyone who did. The directory is
listed at run time, which covers a new example the day it is added.
-}
shippedExamplesSpec :: Spec
shippedExamplesSpec =
    describe "shipped example rulebooks" $
        runIO (listFixtures examplesPath ".yaml") >>= mapM_ loadsTest
  where
    loadsTest fpath =
        it (T.unpack ("loads " <> decodeOsPath fpath)) $ do
            rbPath <- mkAbsolute (examplesPath </> fpath)
            res <- runEff . runFileSystemIO $ ruleBookFromFile rbPath
            first T.unpack res `shouldSatisfy` isRight

examplesPath :: OsPath
examplesPath = [osp|examples/rules|]

--------------------------------------------------------------------------------
-- Glob+ compilation
--------------------------------------------------------------------------------

{- | An invalid pattern must fail when the rulebook is loaded, not silently
turn into a rule that matches the wrong thing. Every message names the rule
and the field it came from, so the author can find it.
-}
globCompilationSpec :: Spec
globCompilationSpec = describe "Glob+ compilation" $ do
    it "compiles a rule with several variables" $
        compileErrorOf (rulebook ["    target: \"@/components/{{provider-name}}/{{service-type}}/{{FileName}}View\"", usesImport "{{TARGET_DIR}}/use{{FileName}}ViewModel"])
            `shouldBe` Nothing

    it "still compiles the legacy single-variable syntax" $
        compileErrorOf (rulebook ["    target: \"@/features/**/{{FileName}}Container\"", usesImport "{{TARGET_DIR}}/{{file-name}}-repository"])
            `shouldBe` Nothing

    it "rejects a clause variable the target never captures" $ do
        let err = compileError (rulebook ["    target: \"@/features/{{FileName}}View\"", usesImport "{{TARGET_DIR}}/{{provider-name}}"])
        err `shouldSatisfy` T.isInfixOf "rule 'a-rule', uses.import"
        err `shouldSatisfy` T.isInfixOf "unknown variable {{provider-name}}"
        err `shouldSatisfy` T.isInfixOf "bound by this rule's target: file-name"

    it "rejects an ambiguous single-word variable in a target" $ do
        let err = compileError (rulebook ["    target: \"@/components/{{provider}}/index\""])
        err `shouldSatisfy` T.isInfixOf "rule 'a-rule', target"
        err `shouldSatisfy` T.isInfixOf "camelCase and kebab-case"

    it "rejects a variable in an exclude pattern" $ do
        let err = compileError (rulebook ["    target: \"@/features/**\"", "    exclude:", "      - \"@/features/**/{{FileName}}.spec\""])
        err `shouldSatisfy` T.isInfixOf "rule 'a-rule', exclude"
        err `shouldSatisfy` T.isInfixOf "exclude pattern"

    it "rejects {{TARGET_DIR}} in a target pattern" $
        compileError (rulebook ["    target: \"{{TARGET_DIR}}/index\""])
            `shouldSatisfy` T.isInfixOf "cannot be used in a target pattern"

    it "rejects a misspelled {{TARGET_DIR}} in a clause" $
        compileError (rulebook ["    target: \"@/features/**\"", usesImport "{{targetDir}}/index"])
            `shouldSatisfy` T.isInfixOf "is reserved"

    it "rejects two adjacent variables in a target" $
        compileError (rulebook ["    target: \"@/x/{{FileName}}{{ServiceType}}\""])
            `shouldSatisfy` T.isInfixOf "no boundary between the two variables"

    it "reports the first failing rule and names it" $
        compileError (twoRules "@/ok/**" "@/x/{{provider}}")
            `shouldSatisfy` T.isInfixOf "rule 'second'"

-- Helpers

usesImport :: Text -> Text
usesImport glob = "    uses:\n      - import: \"" <> glob <> "\""

-- | A one-rule rulebook whose target and clauses are supplied by the caller.
rulebook :: [Text] -> ByteString
rulebook ruleLines =
    encodeUtf8 . T.unlines $
        ["id: rb", "name: Rulebook", "description: d", "rules:", "  - id: a-rule", "    description: d"]
            <> ruleLines
            <> ["    fix: f"]

twoRules :: Text -> Text -> ByteString
twoRules firstTarget secondTarget =
    encodeUtf8 . T.unlines $
        [ "id: rb"
        , "name: Rulebook"
        , "description: d"
        , "rules:"
        , "  - id: first"
        , "    description: d"
        , "    target: \"" <> firstTarget <> "\""
        , "    fix: f"
        , "  - id: second"
        , "    description: d"
        , "    target: \"" <> secondTarget <> "\""
        , "    fix: f"
        ]

compileErrorOf :: ByteString -> Maybe Text
compileErrorOf = leftToMaybe . (ruleBookFromDto <=< first T.pack . parseRuleBookYaml)

compileError :: ByteString -> Text
compileError = fromMaybe "<compiled successfully>" . compileErrorOf
