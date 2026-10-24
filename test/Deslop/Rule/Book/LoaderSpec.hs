module Deslop.Rule.Book.LoaderSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.GlobPlus (CompiledClausePattern (..), CompiledExcludePattern (..), CompiledTargetPattern (..), Polarity (..), VarName (..))
import Deslop.GlobPlus.Compiler (GlobPlusError (..))
import Deslop.Rule.Book (AllowsClause (..), ExistsClause (..), ForbidsClause (..), Rule (..), RuleId (..), Rulebook (..), RulebookId (..), UsesClause (..))
import Deslop.Rule.Book.Compiler (CompileError (..), Field (..), renderCompileError)
import Deslop.Rule.Book.Loader (RulebookLoadError (..), loadRulebookFromFile, loadRulebooksFromDir)
import Effectful (runEff)
import Effects.FileSystem (runFileSystemIO)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, encodeOsPathString, withAbsBaseUnsafe)
import Fixtures.Deslop.Rule.Book (loadRulebookBytes)
import Fixtures.Deslop.Rule.Book.Dto (rulebookYaml)
import Fixtures.OnDisk (rulebooks)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import TestUtils (listFixtures, mkAbsolute)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "Deslop.Rule.Book.Loader" $ do
    rulebookFilesSpec
    rulesDirSpec
    shippedExamplesSpec
    globCompilationSpec
    kebabKeySpec

--------------------------------------------------------------------------------
-- Rulebook files
--------------------------------------------------------------------------------

{- | Each rulebook fixture, loaded off disk. What a valid one loads to is
written out as an outline of every rule - its target and each clause, with the
polarity and reach the compiler fixed for it - so that what a piece of sugar
desugars to is read off the spec, not off a dump of the compiled patterns.
-}
rulebookFilesSpec :: Spec
rulebookFilesSpec = describe "loadRulebookFromFile" $ do
    it "loads every rulebook fixture this spec knows about" $ do
        files <- listFixtures rulebooks ".yaml"

        sort (fmap decodeOsPath files) `shouldBe` ["invalid-globs.yaml", "invalid.yaml", "page-architecture.yaml", "syntax-sugar.yaml"]

    it "reports a rulebook that is not shaped like one, naming the missing key" $
        loadFixture [osp|invalid.yaml|]
            >>= (`shouldBe` Left (UnreadableYaml missingDescription)) . fmap outline

    it "reports a clause naming a variable its target never binds, with the variables in scope" $
        loadFixture [osp|invalid-globs.yaml|]
            >>= (`shouldBe` Left (UncompilablePatterns (one unboundProviderName))) . fmap outline

    it "loads the page architecture rulebook" $ do
        loaded <- loadFixture [osp|page-architecture.yaml|]

        fmap header loaded `shouldBe` Right (RulebookId "page-arch", "Page Architecture", "Architectural rules to build web pages")
        fmap outline loaded
            `shouldBe` Right
                [ ("next-js-page-on-server", ["target @/app/**/page", "example"])
                , ("page-container", ["target @/app/**/page", "uses @/features/**/*Container (narrow, direct)"])
                ,
                    ( "page-container-wires-view-and-viewmodel"
                    ,
                        [ "target @/features/**/{{FileName}}Container"
                        , "uses {{TARGET_DIR}}/{{FileName}}StateEvent (narrow, direct)"
                        , "uses {{TARGET_DIR}}/use{{FileName}}ViewModel (narrow, direct)"
                        , "uses {{TARGET_DIR}}/{{FileName}}View (narrow, transitive)"
                        , "example"
                        ]
                    )
                ,
                    ( "viewmodel"
                    ,
                        [ "target @/features/**/use{{FileName}}ViewModel"
                        , "forbids {{TARGET_DIR}}/{{FileName}}View (widen, direct)"
                        , "forbids @/**/components/**/* (widen, direct)"
                        , "uses {{TARGET_DIR}}/{{FileName}}StateEvent (narrow, direct)"
                        , "example"
                        ]
                    )
                ,
                    ( "viewmodel-test"
                    ,
                        [ "target @/features/**/use{{FileName}}ViewModel"
                        , "uses {{TARGET_DIR}}/use{{FileName}}ViewModel (narrow, direct)"
                        , "exists {{TARGET_DIR}}/use{{FileName}}ViewModel.spec (narrow)"
                        , "example"
                        ]
                    )
                ,
                    ( "page-view"
                    ,
                        [ "target @/features/**/{{FileName}}View"
                        , "uses {{TARGET_DIR}}/{{FileName}}StateEvent (narrow, direct)"
                        , "example"
                        ]
                    )
                ,
                    ( "page-view-storybook"
                    ,
                        [ "target @/features/**/{{FileName}}View"
                        , "exists {{TARGET_DIR}}/{{FileName}}View.stories (narrow)"
                        , "example"
                        ]
                    )
                ,
                    ( "features-cant-import-other-features"
                    ,
                        [ "target @/features/**"
                        , "forbids @/features/** (widen, direct)"
                        , "allows {{TARGET_DIR}}/** (narrow)"
                        ]
                    )
                ]

    -- This repository's answer to -ddump-ds: what each piece of sugar becomes.
    it "loads every piece of syntax sugar as what it desugars to" $ do
        loaded <- loadFixture [osp|syntax-sugar.yaml|]

        fmap outline loaded
            `shouldBe` Right
                [
                    ( "allows-only-alone"
                    ,
                        [ "target @/client/{{feature-name}}/**"
                        , "forbids ** (widen, direct)"
                        , "allows {{TARGET_DIR}}/** (narrow)"
                        ]
                    )
                ,
                    ( "allows-only-beside-hand-written-clauses"
                    ,
                        [ "target @/client/{{feature-name}}/{{FileName}}View"
                        , "forbids @/server/** (widen, transitive)"
                        , "forbids ** (widen, direct)"
                        , "allows @/lib/** (narrow)"
                        , "allows {{TARGET_DIR}}/** (narrow)"
                        ]
                    )
                ,
                    ( "parent-dir-star-reaches-any-ancestor"
                    ,
                        [ "target @/client/**/{{FileName}}View"
                        , "forbids ** (widen, direct)"
                        , "allows {{TARGET_DIR}}/** (narrow)"
                        , "allows {{TARGET_DIR}}/..*/shared/** (narrow)"
                        ]
                    )
                ,
                    ( "parent-dir-star-in-a-uses-clause"
                    ,
                        [ "target @/client/**/{{FileName}}Container"
                        , "uses {{TARGET_DIR}}/..*/shared/registry (narrow, direct)"
                        ]
                    )
                ]
  where
    loadFixture :: OsPath -> IO (Either RulebookLoadError Rulebook)
    loadFixture file = do
        path <- mkAbsolute (rulebooks </> file)
        runEff . runFileSystemIO $ loadRulebookFromFile path

    header :: Rulebook -> (RulebookId, Text, Text)
    header book = (book.id, book.name, book.description)

    missingDescription =
        "AesonException \"Error in $: parsing Deslop.Rule.Book.Dto.RulebookDto(RulebookDto) failed, key \\\"description\\\" not found\""

    unboundProviderName =
        CompileError
            { rule = RuleId "provider-service-view"
            , field = UsesField
            , glob = "{{TARGET_DIR}}/{{provider-nam}}Service"
            , cause =
                UnboundVariable
                    (VarName "provider-nam")
                    (Set.fromList [VarName "file-name", VarName "provider-name", VarName "service-type"])
            }

{- | A rule as the lines of an outline: its target, then every clause in the
order the rule is enforced, and whether it carries an example.
-}
outline :: Rulebook -> [(Text, [Text])]
outline book = ruleOutline <$> book.rules
  where
    ruleOutline rule =
        ( let RuleId ruleId = rule.id in ruleId
        , ["target " <> rule.target.source]
            <> (("exclude " <>) . (.source) <$> clauses rule.exclude)
            <> (forbidsLine <$> clauses rule.forbids)
            <> (allowsLine <$> clauses rule.allows)
            <> (usesLine <$> clauses rule.uses)
            <> (existsLine <$> clauses rule.exists)
            <> ["example" | isJust rule.example]
        )

    clauses :: Maybe (NonEmpty a) -> [a]
    clauses = maybe [] toList

    forbidsLine (ForbidsImport target transitive) = "forbids " <> clause target <> reach transitive
    allowsLine (AllowsImport target) = "allows " <> clause target <> ")"
    usesLine (UsesImport target transitive) = "uses " <> clause target <> reach transitive
    existsLine (ExistsModule target) = "exists " <> clause target <> ")"

    clause target = target.source <> " (" <> polarity target.polarity
    reach = bool ", direct)" ", transitive)"

    polarity = \case
        Widen -> "widen"
        Narrow -> "narrow"

--------------------------------------------------------------------------------
-- The rules directory
--------------------------------------------------------------------------------

{- | What 'loadRulebooksFromDir' makes of a directory holding more than
rulebooks. Pinned as it behaves today: every entry is read as a rulebook,
whatever its name.
-}
rulesDirSpec :: Spec
rulesDirSpec = describe "loadRulebooksFromDir" $ do
    it "loads nothing, without failing, from a project with no rules directory" $
        fmap (fmap outline) <$> inRulesDir (const pass) `shouldReturn` Right []

    it "loads nothing, without failing, from an empty rules directory" $
        fmap (fmap outline) <$> inRulesDir (SDO.createDirectoryIfMissing True) `shouldReturn` Right []

    it "loads every rulebook in it, in file name order" $ do
        loaded <- inRulesDir $ \dir -> do
            SDO.createDirectoryIfMissing True dir
            SFO.writeFile' (dir </> [osp|b.yaml|]) (rulebookYaml ["    target: \"@/b/**\""])
            SFO.writeFile' (dir </> [osp|a.yaml|]) (rulebookYaml ["    target: \"@/a/**\""])

        fmap (fmap outline) loaded `shouldBe` Right [[("a-rule", ["target @/a/**"])], [("a-rule", ["target @/b/**"])]]

    -- Pins a known bug: a README beside the rulebooks aborts the run. Closing
    -- #247 makes this red, which is the point of writing it down.
    it "still reads a file that is not .yaml as a rulebook, and fails on it (known bug, #247)" $ do
        loaded <- inRulesDir $ \dir -> do
            SDO.createDirectoryIfMissing True dir
            SFO.writeFile' (dir </> [osp|a.yaml|]) (rulebookYaml ["    target: \"@/a/**\""])
            SFO.writeFile' (dir </> [osp|README.md|]) "Rules for this project."

        loaded `shouldSatisfy` \case
            Left report -> "Could not load 1 rulebook." `T.isPrefixOf` report && "deslop/rules/README.md" `T.isInfixOf` report
            Right _ -> False

    -- Pins a known bug: a subdirectory escapes as a raw IO exception rather
    -- than a load error. Closing #248 makes this red.
    it "still fails outright on a subdirectory, which it tries to read as a file (known bug, #248)" $
        inRulesDir (\dir -> SDO.createDirectoryIfMissing True (dir </> [osp|nested|]))
            `shouldThrow` anyIOException
  where
    -- Loads the rules directory of a fresh project, prepared by the action
    -- given the directory's path.
    inRulesDir :: (OsPath -> IO ()) -> IO (Either Text [Rulebook])
    inRulesDir prepare = withSystemTempDirectory "deslop-rules" $ \tmp -> do
        root <- mkAbsolute (encodeOsPathString tmp)
        let dir = withAbsBaseUnsafe root [osp|deslop/rules|]
        prepare dir.osPath
        runEff . runFileSystemIO $ loadRulebooksFromDir (ProjectRoot root) dir

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
            res <- runEff . runFileSystemIO $ loadRulebookFromFile rbPath
            res `shouldSatisfy` isRight

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
        compileErrorOf (rulebookYaml ["    target: \"@/components/{{provider-name}}/{{service-type}}/{{FileName}}View\"", usesImport "{{TARGET_DIR}}/use{{FileName}}ViewModel"])
            `shouldBe` Nothing

    it "still compiles the legacy single-variable syntax" $
        compileErrorOf (rulebookYaml ["    target: \"@/features/**/{{FileName}}Container\"", usesImport "{{TARGET_DIR}}/{{file-name}}-repository"])
            `shouldBe` Nothing

    it "rejects a clause variable the target never captures" $ do
        let err = compileError (rulebookYaml ["    target: \"@/features/{{FileName}}View\"", usesImport "{{TARGET_DIR}}/{{provider-name}}"])
        err `shouldSatisfy` T.isInfixOf "rule 'a-rule'"
        err `shouldSatisfy` T.isInfixOf "uses.import"
        err `shouldSatisfy` T.isInfixOf "unknown variable {{provider-name}}"
        err `shouldSatisfy` T.isInfixOf "bound by this rule's target: file-name"

    it "rejects an ambiguous single-word variable in a target" $ do
        let err = compileError (rulebookYaml ["    target: \"@/components/{{provider}}/index\""])
        err `shouldSatisfy` T.isInfixOf "rule 'a-rule'"
        err `shouldSatisfy` T.isInfixOf "target:"
        err `shouldSatisfy` T.isInfixOf "camelCase and kebab-case"

    it "rejects a variable in an exclude pattern" $ do
        let err = compileError (rulebookYaml ["    target: \"@/features/**\"", "    exclude:", "      - \"@/features/**/{{FileName}}.spec\""])
        err `shouldSatisfy` T.isInfixOf "rule 'a-rule'"
        err `shouldSatisfy` T.isInfixOf "exclude:"
        err `shouldSatisfy` T.isInfixOf "exclude pattern"

    it "rejects {{TARGET_DIR}} in a target pattern" $
        compileError (rulebookYaml ["    target: \"{{TARGET_DIR}}/index\""])
            `shouldSatisfy` T.isInfixOf "cannot be used in a target pattern"

    it "rejects a misspelled {{TARGET_DIR}} in a clause" $
        compileError (rulebookYaml ["    target: \"@/features/**\"", usesImport "{{targetDir}}/index"])
            `shouldSatisfy` T.isInfixOf "is reserved"

    it "rejects two adjacent variables in a target" $
        compileError (rulebookYaml ["    target: \"@/x/{{FileName}}{{ServiceType}}\""])
            `shouldSatisfy` T.isInfixOf "no literal between the two variables"

    it "rejects two variables separated only by a star, which can match nothing" $
        compileError (rulebookYaml ["    target: \"@/x/{{FileName}}*{{ServiceType}}\""])
            `shouldSatisfy` T.isInfixOf "no literal between the two variables"

    it "rejects a variable standing between two globstars" $
        compileError (rulebookYaml ["    target: \"@/**/{{provider-name}}/**\""])
            `shouldSatisfy` T.isInfixOf "has ** on both sides"

    it "rejects a globstar glued to text inside a segment" $
        compileError (rulebookYaml ["    target: \"@/x/**View\""])
            `shouldSatisfy` T.isInfixOf "glues ** to text"

    it "names the failing rule among rules that compile" $
        compileError (twoRules "@/ok/**" "@/x/{{provider}}")
            `shouldSatisfy` T.isInfixOf "rule 'second'"

    it "reports every failing rule, not only the first" $ do
        let err = compileError (twoRules "@/x/{{provider}}" "@/y/**View")
        err `shouldSatisfy` T.isInfixOf "rule 'first'"
        err `shouldSatisfy` T.isInfixOf "rule 'second'"

    it "stays quiet about a rule's clauses when its target did not compile" $ do
        -- There is no scope to check them against, so blaming each clause for a
        -- variable the target never got to bind would be noise.
        let err = compileError (rulebookYaml ["    target: \"@/x/{{provider}}\"", usesImport "{{TARGET_DIR}}/{{FileName}}"])
        err `shouldSatisfy` T.isInfixOf "target:"
        err `shouldNotSatisfy` T.isInfixOf "uses.import"

{- | A rulebook key of two or more words is kebab-case, and the camelCase
spelling is a parse error rather than a clause quietly dropped - see
"Deslop.Rule.Book.DtoSpec". Pinned from this end too, because what the rest of
the pipeline sees is the whole point: the right spelling has to reach the
desugarer, and the wrong one has to never reach it at all.
-}
kebabKeySpec :: Spec
kebabKeySpec = describe "multi-word keys" $ do
    it "reads allows-only, and desugars it" $
        forbidsOf (rulebookYaml ["    target: \"@/x/**\"", allowsOnlyImport "allows-only" "@/shared/**"])
            `shouldBe` Just ["**"]

    it "does not read the camelCase spelling of it" $
        forbidsOf (rulebookYaml ["    target: \"@/x/**\"", allowsOnlyImport "allowsOnly" "@/shared/**"])
            `shouldBe` Nothing

-- | Every @forbids@ glob of a one-rule rulebook, once loaded.
forbidsOf :: ByteString -> Maybe [Text]
forbidsOf bytes = do
    book <- rightToMaybe (loadRulebookBytes bytes)
    rule <- viaNonEmpty head book.rules
    pure [clause.target.source | clause <- maybe [] toList rule.forbids]

-- Helpers

allowsOnlyImport :: Text -> Text -> Text
allowsOnlyImport key glob = "    " <> key <> ":\n      - import: \"" <> glob <> "\""

usesImport :: Text -> Text
usesImport glob = "    uses:\n      - import: \"" <> glob <> "\""

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

{- | The rendered failures of a rulebook, as a user would read them, minus the
file heading the loader adds.
-}
compileErrorOf :: ByteString -> Maybe Text
compileErrorOf = fmap renderAll . leftToMaybe . loadRulebookBytes
  where
    renderAll = \case
        UncompilablePatterns errors -> T.intercalate "\n" . fmap renderCompileError . toList $ errors
        UnreadableYaml detail -> "unreadable: " <> detail

compileError :: ByteString -> Text
compileError = fromMaybe "<compiled successfully>" . compileErrorOf
