module E2E.ProjectGoldenSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop (doWork)
import Deslop.Error (DeslopError (..))
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.RunReport (RunReport (..), Verdict (..))
import Doubles.Browser (runBrowserInMemory, unattended)
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, renderTranscript, runMockCLI)
import Doubles.FileSystem (runWritesInMemory)
import Doubles.Git (Repository (..), noChanges, runGitInMemory)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.CLI (LogStyle (..))
import Effects.FileSystem (runFileSystemIO, runRoFileSystemIO)
import Effects.Git (GitRef (..), NoPullRequest (..))
import Effects.ReportProblem (runReportProblem)
import FileSystem.Path (
    AbsPath (osPath),
    ProjectRelativePath (osPath),
    ProjectRoot (..),
    decodeOsPath,
    encodeOsPathString,
    relativePathTo,
    withAbsBaseUnsafe,
 )
import Fixtures.Deslop.Explore.Html (graphIn)
import Fixtures.Deslop.Explore.ModuleGraph.Json (moduleTags, textField)
import Fixtures.OnDisk (Language (..), inCopyOf, languageDir, projectIn)
import Frontend.TypeScript (TsFrontend (..))
import Git.Ignore (loadGitIgnore)
import Git.SourceLink (Branch (..), DiffAnchors (..), Forge (..), PullRequest (..), Revision (..), SourceBase (..), SourceLinks (..))
import Params (Command (..), Exploration (..), Opening (..), Params (..))
import Renderable qualified
import System.Directory.OsPath (removeFile)
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import Test.Hspec.Golden (Golden, defaultGolden)
import TestUtils (mkAbsolute, requireJust, rp, snapshot)
import UI (coverage, humanReadable, problemsFoundText)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "E2E.Project" $ do
    itChecks TypeScript "ts-project-1"
    itChecks TypeScript "ixartz-next-js-boilerplate"
    itChecks TypeScript "melzar-nextjs-clean-architecture"
    itChecks TypeScript "ts-cycles-project"
    itChecks TypeScript "ts-gitignore-project"
    itChecks TypeScript "ts-globplus-project"
    itChecks TypeScript "ts-casing-project"
    itChecks TypeScript "ts-monorepo-project"
    itChecks TypeScript "ts-barrel-project"
    itChecks TypeScript "ts-unscanned-project"
    itChecks TypeScript "ts-not-utf8-project"

    itFailsToLoadRulebook TypeScript "ts-invalid-rulebook-project"
    itFailsToLoadConfig TypeScript "tsconfig-error" "ts-broken-extends-project" "config/tsconfig.base.json"

    itChecks Haskell "hs-tiny-project"
    itBaselines Haskell "hs-tiny-project"
    -- Haskell has no built-in rule that rewrites code, so fix changes nothing.
    itFixes Haskell "hs-tiny-project" ["src/Tiny/Core.hs", "app/Main.hs"]
    itFailsToLoadConfig Haskell "cabal-error" "hs-multi-package-project" "cabal.project"
    itFailsToLoadConfig Haskell "cabal-error" "hs-two-packages-project" "a.cabal"
    itChecksItself

    itChecks Nix "nix-tiny-project"
    itBaselines Nix "nix-tiny-project"
    -- Nix has no built-in rule that rewrites code, so fix changes nothing.
    itFixes Nix "nix-tiny-project" ["flake.nix", "packages/default.nix"]
    itChecks Nix "nix-nixos-config"
    itBaselines Nix "nix-nixos-config"
    itFailsToLoadConfig Nix "nix-config-error" "nix-broken-config-project" "deslop/nix.yaml"

    itFindsNoLanguage
    itFindsNoLanguageInMissingPath
    itReportsRulebookBeforeLanguage "ts-invalid-rulebook-project"

    itBaselines TypeScript "ts-project-1"
    itBaselines TypeScript "ixartz-next-js-boilerplate"
    itBaselines TypeScript "melzar-nextjs-clean-architecture"
    itBaselines TypeScript "ts-cycles-project"
    itBaselines TypeScript "ts-gitignore-project"
    itBaselines TypeScript "ts-globplus-project"
    itBaselines TypeScript "ts-casing-project"
    itBaselines TypeScript "ts-monorepo-project"
    itBaselines TypeScript "ts-barrel-project"
    itBaselines TypeScript "ts-unscanned-project"
    itBaselines TypeScript "ts-not-utf8-project"

    itIterates "ts-gitignore-project"

    itExplores "ixartz-next-js-boilerplate"
    itExploresWithoutPullRequest GhMissing
    itExploresWithoutPullRequest NoneFound
    itExploresDetachedWithoutAskingForPullRequest
    itAsksNoPullRequestWithoutChanges
    itAsksGitNothing CheckC
    itAsksGitNothing FixC
    itAsksGitNothing BaselineC

    itFixes
        TypeScript
        "ts-barrel-project"
        [ "src/lib/index.ts"
        , "src/lib/literals.ts" -- statement-shaped text, must not be rewritten
        , "src/lib/tricky.tsx" -- real dependencies written after text that could swallow them
        ]

    itFixes
        TypeScript
        "ts-project-1"
        [ "src/app/[locale]/login/page.tsx"
        , "src/features/home/home-screen.tsx"
        , "src/features/home/home-component.ts"
        , "src/features/home/home.spec.ts"
        , "src/app/[locale]/login/page.tsx"
        , "src/features/login/login.spec.ts"
        , "src/features/login/login-form.ts"
        , "tests/fixtures.ts"
        , "vitest.config.ts"
        , "next.config.ts"
        , "next.config.spec.ts"
        , "src/lib/util.ts" -- baseline file should not be changed
        ]

    itFixes
        TypeScript
        "ixartz-next-js-boilerplate"
        [ ".storybook/preview.ts"
        , "next.config.ts"
        , "src/components/Hello.tsx"
        , "src/libs/DB.ts"
        , "src/libs/I18n.ts"
        , "src/libs/I18nNavigation.ts"
        , "src/libs/Logger.ts"
        , "src/proxy.ts"
        , "src/templates/BaseTemplate.stories.tsx"
        , "src/templates/BaseTemplate.test.tsx"
        , "src/utils/Helpers.test.ts"
        ]

    itFixes
        TypeScript
        "melzar-nextjs-clean-architecture"
        [ "src/app/layout.tsx"
        , "src/ui/common/components/layout/ContainerBox/ContainerBox.tsx"
        , "src/ui/common/components/layout/TopHeader/TopHeader.stories.tsx"
        , "src/ui/common/components/layout/TopNavigation/TopNavigation.tsx"
        , "tsconfig.json"
        , "src/middleware.ts"
        , "src/app/page.tsx"
        ]

    itFixes
        TypeScript
        "ts-monorepo-project"
        [ "packages/app/src/main.ts"
        , "packages/app/src/checkout.ts"
        ]

    -- What the .gitignore files re-include is repaired; what they ignore is
    -- never walked, so it keeps its relative imports as written.
    itFixes
        TypeScript
        "ts-gitignore-project"
        [ "src/bad.ts"
        , "src/features/sub/scratch.ts"
        , "src/keep.gen.ts"
        , "src/other.gen.ts"
        , "src/features/scratch.ts" -- ignored
        , "generated/model.ts" -- ignored
        , "vendor/legacy.ts" -- ignored
        ]

    -- legacy.ts reaches the gitignored src/vendor/ relatively; fix still aliases it.
    itFixes TypeScript "ts-unscanned-project" ["src/app/legacy.ts", "src/app/page.ts"]

    -- Projects whose only problems are Rule Violations, which fix never touches.
    itFixes TypeScript "ts-casing-project" ["src/components/aws-s3/AWSS3View.tsx", "src/config/a-b.ts"]
    itFixes TypeScript "ts-globplus-project" ["src/application/archive-order/ArchiveOrderUseCase.ts", "src/components/paypal.ts"]
    itFixes TypeScript "ts-cycles-project" ["src/a.ts", "src/b.ts"]
    itFixes TypeScript "ts-not-utf8-project" ["src/app/page.ts"]
    itLeavesUnreadFilesAlone TypeScript "ts-not-utf8-project" [osp|src/app/legacy.ts|]

--------------------------------------------------------------------------------
-- Running Deslop
--------------------------------------------------------------------------------

{- | Where a run's writes go. A fixture read in place must never be rewritten,
so its writes are kept in memory and asserted on; a run that is meant to
rewrite files works on a copy of the fixture, on disk.
-}
data Writes
    = InMemory
    | ToDisk

-- | Everything one 'doWork' leaves behind.
data Run = Run
    { result :: Either DeslopError RunReport
    , logs :: TestLogs
    , written :: Map AbsPath ByteString
    -- ^ What an 'InMemory' run wrote. A run 'ToDisk' leaves its writes on disk.
    }

-- | Runs one command over the project at the path, collecting what it leaves behind.
runDeslopOn :: Writes -> Command -> OsPath -> IO Run
runDeslopOn = runDeslopIn noChanges

-- | As 'runDeslopOn', in this repository.
runDeslopIn :: Repository -> Writes -> Command -> OsPath -> IO Run
runDeslopIn repo writes command projectPath = do
    absProjectPath <- mkAbsolute projectPath
    writtenRef <- newIORef mempty
    logsRef <- newIORef (TestLogs [])
    result <-
        runEff
            . fileSystem writtenRef
            . runErrorNoCallStack @DeslopError
            . runMockCLI defaultMockCLI {logsRef = Just logsRef}
            . runReportProblem
            . runConcurrent
            . runGitInMemory repo
            . runBrowserInMemory unattended
            $ doWork Params {projectPath = absProjectPath, command}
    Run result <$> readIORef logsRef <*> readIORef writtenRef
  where
    fileSystem writtenRef = case writes of
        InMemory -> runRoFileSystemIO . runWritesInMemory writtenRef
        ToDisk -> runFileSystemIO

{- | As 'runDeslopOn', over a copy of the fixture project in a fresh temporary
directory, which the run may rewrite. The copy is prepared before the run and
read back, by its root, after it.
-}
runDeslopOnCopy :: Command -> Language -> String -> (OsPath -> IO ()) -> (OsPath -> Run -> IO a) -> IO a
runDeslopOnCopy command language project prepare check =
    inCopyOf language project $ \tmpDir -> do
        prepare tmpDir
        runDeslopOn ToDisk command tmpDir >>= check tmpDir

-- | Runs against a fresh, empty temporary directory.
inEmptyDir :: (OsPath -> IO a) -> IO a
inEmptyDir act = withSystemTempDirectory "deslop-test" (act . encodeOsPathString)

projectGolden :: Language -> String -> String -> String -> Golden String
projectGolden language kind project = defaultGolden (languageDir language <> "/" <> kind <> "-" <> project)

{- | The closing summary and the check verdict are rendered by runDeslop,
outside doWork, so the transcript alone would not cover them. Appending them
goldens the wording together with the counts that produced it. Only the
elapsed time is left out, since it differs on every run.
-}
renderRun :: Run -> Text
renderRun run = renderTranscript run.logs <> renderResult run.result
  where
    renderResult (Left err) = "[exit] " <> humanReadable err <> "\n"
    renderResult (Right report) =
        "[summary] " <> coverage report.summary <> "\n" <> renderVerdict report.verdict

    renderVerdict Clean = ""
    renderVerdict (ProblemsFound counts) =
        "[exit] " <> problemsFoundText counts <> "\n"

--------------------------------------------------------------------------------
-- Cases
--------------------------------------------------------------------------------

itChecks :: Language -> String -> Spec
itChecks language project = it ("checks " <> project) $ do
    run <- runDeslopOn InMemory CheckC (projectIn language project)

    run.written `shouldBe` mempty
    pure . projectGolden language "check" project . T.unpack . renderRun $ run

{- | A baseline is committed and read back on another machine and in CI, so an
entry naming the machine that wrote it matches nothing there: the problem it
was meant to suppress comes back, and @deslop fix@ rewrites an import a
teammate had already accepted. Asserted on every fixture rather than
goldened, because a golden only shows the path - it cannot object to it. A
'/'-rooted Module Id is fine and expected; what must never appear is where the
project happens to sit on this disk.
-}
itBaselines :: Language -> String -> Spec
itBaselines language project = it ("baselines " <> project) $ do
    let projectPath = projectIn language project
    absProjectPath <- mkAbsolute projectPath
    let baselinePath = withAbsBaseUnsafe absProjectPath [osp|deslop/baseline.yaml|]

    run <- runDeslopOn InMemory BaselineC projectPath

    fmap (.verdict) run.result `shouldBe` Right Clean
    Map.keys run.written `shouldBe` [baselinePath]
    content <- requireJust "Expected deslop/baseline.yaml to be written" $ Map.lookup baselinePath run.written
    traverse_ (`shouldSatisfy` namesNoMachine absProjectPath) . baselineEntries $ content
    pure . projectGolden language "baseline" project . T.unpack $
        renderRun run <> "\n>>> baseline.yaml\n" <> TE.decodeUtf8 content
  where
    baselineEntries :: ByteString -> [Text]
    baselineEntries = mapMaybe (T.stripPrefix "- " . T.strip) . lines . TE.decodeUtf8

    namesNoMachine :: AbsPath -> Text -> Bool
    namesNoMachine projectPath = not . T.isInfixOf (decodeOsPath projectPath.osPath)

itFixes :: Language -> String -> [String] -> Spec
itFixes language project filesToCheck = it ("fixes " <> project) . runDeslopOnCopy FixC language project (const pass) $ \tmpDir run -> do
    fmap (.verdict) run.result `shouldBe` Right Clean
    let modified = [msg | (Change, msg) <- run.logs.transcript]
    -- Files are repaired concurrently; the changelog must not say so.
    modified `shouldBe` sort modified
    fullSnapshot <- snapshot tmpDir filesToCheck
    pure . projectGolden language "fix" project $
        fullSnapshot <> "\n>>> changelog\n" <> T.unpack (T.unlines modified)

{- | A file Deslop cannot read is kept as its bytes, so @deslop fix@ writes
them back unchanged - not one byte re-encoded.
-}
itLeavesUnreadFilesAlone :: Language -> String -> OsPath -> Spec
itLeavesUnreadFilesAlone language project file =
    it ("fixes " <> project <> " without re-encoding the file it cannot read") . runDeslopOnCopy FixC language project (const pass) $ \tmpDir run -> do
        original <- SFO.readFile' (projectIn language project </> file)

        fmap (.verdict) run.result `shouldBe` Right Clean
        SFO.readFile' (tmpDir </> file) `shouldReturn` original

{- | Goldens exactly which files the walk produced. Unlike the check and
baseline goldens, which can only show a skipped file as an absence, this
states the outcome positively: a regression makes a named path appear. Goes
through the TypeScript frontend's own stages, strung together as the driver
strings them.
-}
itIterates :: String -> Spec
itIterates project = it ("iterates " <> project) $ do
    projectRoot <- ProjectRoot <$> mkAbsolute (projectIn TypeScript project)

    files <- runEff . runRoFileSystemIO $ do
        (cfg, _) <- either (error . Renderable.render) id <$> loadConfig TsFrontend projectRoot
        gitIgnore <- loadGitIgnore (ecosystemIgnored TsFrontend cfg) projectRoot.path
        discover TsFrontend cfg projectRoot gitIgnore

    pure . projectGolden TypeScript "iterated" project . T.unpack . T.unlines . sort $
        fmap (decodeOsPath . (.osPath) . relativePathTo projectRoot) files

{- | A rulebook deslop cannot use must abort the run before any file is
checked, with a message the author can act on. Goldening the transcript pins
the whole path from the rulebook file through to the exit line. The fixture
holds both ways a rulebook can be unusable, because the report has to survive
the mixture: two files whose patterns do not compile, and two that never
parsed at all because of an unknown key.
-}
itFailsToLoadRulebook :: Language -> String -> Spec
itFailsToLoadRulebook language project = it ("refuses to run " <> project) $ do
    run <- runDeslopOn InMemory CheckC (projectIn language project)

    run.result `shouldSatisfy` isRulebookError
    run.written `shouldBe` mempty
    pure . projectGolden language "rulebook-error" project . T.unpack . renderRun $ run

{- | A project configuration Deslop cannot use must abort the run before any
file is checked, naming the file at fault: a tsconfig whose @extends@ names a
file that is not there, a cabal.project of several packages, two .cabal files,
a @deslop/nix.yaml@ that is not YAML.
-}
itFailsToLoadConfig :: Language -> String -> String -> Text -> Spec
itFailsToLoadConfig language kind project culprit = it ("refuses to run " <> project) $ do
    run <- runDeslopOn InMemory CheckC (projectIn language project)

    run.result `shouldSatisfy` \case
        Left (FrontendError message) -> culprit `T.isInfixOf` message
        _ -> False
    run.written `shouldBe` mempty
    pure . projectGolden language kind project . T.unpack . renderRun $ run

{- | A directory no frontend recognises cannot be checked at all; saying so
names what was looked for, rather than complaining about one language's config
file in a project that was never written in it.
-}
itFindsNoLanguage :: Spec
itFindsNoLanguage = it "refuses to run a project no language recognises" . inEmptyDir $ \tmpDir -> do
    projectRoot <- ProjectRoot <$> mkAbsolute tmpDir

    run <- runDeslopOn ToDisk CheckC tmpDir

    run.result `shouldBe` Left (NoFrontendDetected projectRoot ["TypeScript (tsconfig.json)", "Haskell (*.cabal)", "Nix (flake.nix or default.nix)"])
    -- The title and the warning every run without Rulebooks gives, but no
    -- claim to have detected a language.
    fmap fst run.logs.transcript `shouldBe` [Title, Warning]

{- | A path that is not a directory holds no tsconfig.json: the person is told
no language matched, not shown a raw IO exception.
-}
itFindsNoLanguageInMissingPath :: Spec
itFindsNoLanguageInMissingPath = it "refuses to run a project path that does not exist" . inEmptyDir $ \tmpDir -> do
    let missing = tmpDir </> [osp|missing|]
    projectRoot <- ProjectRoot <$> mkAbsolute missing

    run <- runDeslopOn ToDisk CheckC missing

    run.result `shouldBe` Left (NoFrontendDetected projectRoot ["TypeScript (tsconfig.json)", "Haskell (*.cabal)", "Nix (flake.nix or default.nix)"])

{- | Detecting the language must not reorder what a broken project hears
first: its Rulebooks are read before any language is required.
-}
itReportsRulebookBeforeLanguage :: String -> Spec
itReportsRulebookBeforeLanguage project =
    it ("reports the broken rulebooks of " <> project <> " before finding no language")
        . runDeslopOnCopy CheckC TypeScript project (removeFile . (</> [osp|tsconfig.json|]))
        $ \_ run -> run.result `shouldSatisfy` isRulebookError

{- | The page carries the graph inline, so a page holding the project's modules
is a page that can show them. How it draws them is the browser's business, not
this suite's.
-}
itExplores :: String -> Spec
itExplores project = it ("explores " <> project) $ do
    let projectPath = projectIn TypeScript project
    absProjectPath <- mkAbsolute projectPath
    let pagePath = withAbsBaseUnsafe absProjectPath [osp|deslop/diagram/index.html|]
        repo = onGitHub {pullRequest = Right $ PullRequest "https://github.com/o/r/pull/7"}

    run <- runDeslopIn repo InMemory explore projectPath

    fmap (.verdict) run.result `shouldBe` Right Clean
    Map.keys run.written `shouldBe` [pagePath]
    page <- decodeUtf8 <$> requireJust "Expected deslop/diagram/index.html to be written" (Map.lookup pagePath run.written)
    graph <- requireJust "Expected the page to carry the graph" (graphIn page)
    textField "schema" graph `shouldBe` Just (Just "deslop-graph/1")
    (find ((== "src/app/[locale]/layout.tsx") . fst) =<< moduleTags graph) `shouldBe` Just ("src/app/[locale]/layout.tsx", ["changed"])
    textField "sourceBase" graph `shouldBe` Just (Just "https://github.com/o/r/blob/main/")
    textField "pullRequest" graph `shouldBe` Just (Just "https://github.com/o/r/pull/7")
    pure . projectGolden TypeScript "explore" project . T.unpack . renderRun $ run

-- | Without its pull request, the explorer is still written, says why once, and links to none.
itExploresWithoutPullRequest :: NoPullRequest -> Spec
itExploresWithoutPullRequest reason = it ("explores without a pull request when " <> show reason) $ do
    run <- runDeslopIn onGitHub {pullRequest = Left reason} InMemory explore (projectIn TypeScript "ts-project-1")

    fmap (.verdict) run.result `shouldBe` Right Clean
    T.count "No pull request to link" (renderRun run) `shouldBe` 1
    pullRequestsOf run `shouldBe` [Nothing]

-- | A detached @HEAD@ has no branch to ask a pull request of, so none is asked for.
itExploresDetachedWithoutAskingForPullRequest :: Spec
itExploresDetachedWithoutAskingForPullRequest = it "explores a detached checkout without asking for a pull request" $ do
    let detached = (\links -> links {revision = Detached "0123abcd"}) <$> onGitHub.sourceLinks
    run <-
        runDeslopIn
            onGitHub {sourceLinks = detached, pullRequest = error "asked for a pull request"}
            InMemory
            explore
            (projectIn TypeScript "ts-project-1")

    fmap (.verdict) run.result `shouldBe` Right Clean
    renderRun run `shouldSatisfy` T.isInfixOf "No pull request to link: no branch is checked out."
    pullRequestsOf run `shouldBe` [Nothing]

-- | A pull request is a network call away, so an explorer with no diff never asks for one.
itAsksNoPullRequestWithoutChanges :: Spec
itAsksNoPullRequestWithoutChanges = it "explores without asking for a pull request when nothing changed" $ do
    run <-
        runDeslopIn
            onGitHub {changes = Map.singleton (GitRef "origin/main") mempty, pullRequest = error "asked for a pull request"}
            InMemory
            explore
            (projectIn TypeScript "ts-project-1")

    fmap (.verdict) run.result `shouldBe` Right Clean
    pullRequestsOf run `shouldBe` [Nothing]

-- | Only explore asks git and gh anything: the other commands stay as fast as they were.
itAsksGitNothing :: Command -> Spec
itAsksGitNothing command = it ("asks git nothing to " <> show command) $ do
    let untouchable =
            Repository
                { changes = error "asked for the changed files"
                , sourceLinks = error "asked for the source links"
                , pullRequest = error "asked for a pull request"
                }

    run <- runDeslopIn untouchable InMemory command (projectIn TypeScript "ts-project-1")

    run.result `shouldSatisfy` isRight

-- | A repository on GitHub, at its root, whose branch changed a file of each fixture project.
onGitHub :: Repository
onGitHub =
    Repository
        { changes = Map.singleton (GitRef "origin/main") (Set.fromList [rp "src/app/[locale]/layout.tsx", rp "README.md", rp "src/index.ts"])
        , sourceLinks =
            Just
                SourceLinks
                    { source = SourceBase "https://github.com/o/r/blob/main/"
                    , revision = OnBranch (Branch "main")
                    , forge = GitHub DiffAnchors {repoPrefix = ""}
                    }
        , pullRequest = Left NoneFound
        }

-- | Explore, marking what changed since @main@.
explore :: Command
explore = ExploreC Exploration {diffBase = GitRef "main", opening = OpenWhenAttended}

-- | The text of every page a run wrote.
pages :: Run -> [Text]
pages = fmap decodeUtf8 . Map.elems . (.written)

-- | The pull request each page written links to, if it carries a graph.
pullRequestsOf :: Run -> [Maybe Text]
pullRequestsOf = mapMaybe (textField "pullRequest" <=< graphIn) . pages

{- | Deslop's own layering (deslop/rules/), judged by its own Haskell frontend.
Asserted clean rather than goldened: a golden would change with every module
the repository gains.
-}
itChecksItself :: Spec
itChecksItself = it "checks its own repository clean" $ do
    run <- runDeslopOn InMemory CheckC [osp|.|]

    (renderRun run, fmap (.verdict) run.result) `shouldSatisfy` ((== Right Clean) . snd)
    run.written `shouldBe` mempty

isRulebookError :: Either DeslopError a -> Bool
isRulebookError = \case
    Left (RulebookError _) -> True
    _ -> False
