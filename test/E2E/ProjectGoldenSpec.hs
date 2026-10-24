module E2E.ProjectGoldenSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop (doWork)
import Deslop.Error (DeslopError (..))
import Deslop.RunReport (RunReport (..), Verdict (..))
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, renderTranscript, runMockCLI)
import Doubles.D2 (FakeD2 (..), runFakeD2)
import Doubles.FileSystem (runMockWrFileSystem)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.CLI (LogStyle (..))
import Effects.FileSystem (runFileSystemIO, runRoFileSystemIO)
import Effects.ReportProblem (runReportProblem)
import FileSystem.Path (
    AbsPath (osPath),
    ProjectRoot (..),
    ProjectRelativePath (osPath),
    decodeOsPath,
    encodeOsPathString,
    relativePathTo,
 )
import Frontend.TypeScript.Ignore (jsEcosystemIgnored)
import Frontend.TypeScript.Iterator (getTsFiles)
import Git.Ignore (loadGitIgnore)
import Params (Command (..), Params (..))
import System.Directory (removeFile)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (copyDir, fixturesPath, mkAbsolute, requireJust, snapshot)
import UI (coverage, humanReadable, problemsFoundText)
import UnliftIO.Temporary (withSystemTempDirectory)

{- | The 'Params' every golden run starts from. Only this spec runs 'doWork'
end to end, so it is the only place that needs one.
-}
defaultParams :: OsPath -> IO Params
defaultParams projPath = do
    absProjPath <- mkAbsolute projPath
    pure
        Params
            { projectPath = absProjPath
            , command = FixC
            }

spec :: Spec
spec = describe "E2E.Project" $ do
    itChecks "ts-project-1"
    itChecks "ixartz-next-js-boilerplate"
    itChecks "melzar-nextjs-clean-architecture"
    itChecks "ts-cycles-project"
    itChecks "ts-gitignore-project"
    itChecks "ts-globplus-project"
    itChecks "ts-casing-project"
    itChecks "ts-monorepo-project"
    itChecks "ts-barrel-project"
    itChecks "ts-unscanned-project"
    itChecks "ts-not-utf8-project"

    itFailsToLoadRulebook "ts-invalid-rulebook-project"
    itFailsToLoadConfig "tsconfig-error-" "ts-broken-extends-project"

    itChecks "hs-tiny-project"
    itBaselines "hs-tiny-project"
    itFailsToLoadConfig "cabal-error-" "hs-multi-package-project"
    itFailsToLoadConfig "cabal-error-" "hs-two-packages-project"
    itChecksItself

    itFindsNoLanguage
    itFindsNoLanguageInMissingPath
    itReportsRulebookBeforeLanguage "ts-invalid-rulebook-project"

    itBaselines "ts-project-1"
    itBaselines "ixartz-next-js-boilerplate"
    itBaselines "melzar-nextjs-clean-architecture"
    itBaselines "ts-cycles-project"
    itBaselines "ts-gitignore-project"
    itBaselines "ts-globplus-project"
    itBaselines "ts-casing-project"
    itBaselines "ts-monorepo-project"
    itBaselines "ts-barrel-project"
    itBaselines "ts-unscanned-project"

    itIterates "ts-gitignore-project"

    itDiagrams "ixartz-next-js-boilerplate"
    itDiagrams "melzar-nextjs-clean-architecture"
    itRefusesToDiagramWithoutD2 "ts-project-1"

    itFixes
        "ts-barrel-project"
        [ "src/lib/index.ts"
        , "src/lib/literals.ts" -- statement-shaped text, must not be rewritten
        , "src/lib/tricky.tsx" -- real dependencies written after text that could swallow them
        ]

    itFixes
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
        "ts-monorepo-project"
        [ "packages/app/src/main.ts"
        , "packages/app/src/checkout.ts"
        ]
  where
    -- A baseline is committed and read back on another machine and in CI, so
    -- an entry naming the machine that wrote it matches nothing there: the
    -- problem it was meant to suppress comes back, and `deslop fix` rewrites
    -- an import a teammate had already accepted. Asserted on every fixture
    -- rather than goldened, because a golden only shows the path - it cannot
    -- object to it. A '/'-rooted Module Id is fine and expected; what must
    -- never appear is where the project happens to sit on this disk.
    baselineEntries :: ByteString -> [Text]
    baselineEntries =
        mapMaybe (T.stripPrefix "- " . T.strip) . lines . TE.decodeUtf8

    namesNoMachine :: AbsPath -> Text -> Bool
    namesNoMachine projectPath = not . T.isInfixOf (decodeOsPath projectPath.osPath)

    -- The closing summary and the check verdict are rendered by runDeslop,
    -- outside doWork, so the transcript alone would not cover them. Appending
    -- them goldens the wording together with the counts that produced it. Only
    -- the elapsed time is left out, since it differs on every run.
    renderResult :: Either DeslopError RunReport -> Text
    renderResult (Left err) = "[exit] " <> humanReadable err <> "\n"
    renderResult (Right report) =
        "[summary] " <> coverage report.summary <> "\n" <> renderVerdict report.verdict

    renderVerdict :: Verdict -> Text
    renderVerdict Clean = ""
    renderVerdict (ProblemsFound counts) =
        "[exit] " <> problemsFoundText counts <> "\n"

    -- Goldens exactly which files the iteration produced. Unlike the check and
    -- baseline goldens, which can only show a skipped file as an absence, this
    -- states the outcome positively: a regression makes a named path appear.
    itIterates project = it ("iterates " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        absProjectPath <- mkAbsolute projectPath

        -- When
        files <-
            runEff . runRoFileSystemIO $
                loadGitIgnore jsEcosystemIgnored absProjectPath >>= (`getTsFiles` absProjectPath)

        -- Then
        pure . defaultGolden ("iterated-" <> project) . T.unpack . T.unlines . sort $
            fmap (decodeOsPath . (.osPath) . relativePathTo (ProjectRoot absProjectPath)) files

    -- A rulebook deslop cannot use must abort the run before any file is
    -- checked, with a message the author can act on. Goldening the transcript
    -- pins the whole path from ruleBookFromDto through to the exit line.
    -- The fixture holds both ways a rulebook can be unusable, because the
    -- report has to survive the mixture: two files whose patterns do not
    -- compile, and two that never parsed at all because of an unknown key.
    itFailsToLoadRulebook project = it ("refuses to run " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef (TestLogs [])
        defParams <- defaultParams projectPath
        let params = defParams {command = CheckC}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                . runFakeD2 D2Missing
                . runReportProblem
                . runConcurrent
                $ doWork params

        -- Then
        res `shouldSatisfy` isLeft
        written <- readIORef filesRef
        written `shouldBe` Nothing
        logs <- readIORef logsRef
        pure . defaultGolden ("rulebook-error-" <> project) . T.unpack $
            renderTranscript logs <> renderResult res

    -- A project configuration Deslop cannot use must abort the run before any
    -- file is checked, naming the file at fault: a tsconfig whose `extends`
    -- names a file that is not there, a cabal.project of several packages.
    itFailsToLoadConfig goldenPrefix project = it ("refuses to run " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef (TestLogs [])
        defParams <- defaultParams projectPath
        let params = defParams {command = CheckC}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                . runFakeD2 D2Missing
                . runReportProblem
                . runConcurrent
                $ doWork params

        -- Then
        res `shouldSatisfy` isLeft
        written <- readIORef filesRef
        written `shouldBe` Nothing
        logs <- readIORef logsRef
        pure . defaultGolden (goldenPrefix <> project) . T.unpack $
            renderTranscript logs <> renderResult res

    -- A directory no frontend recognises cannot be checked at all; saying so
    -- names what was looked for, rather than complaining about one language's
    -- config file in a project that was never written in it.
    itFindsNoLanguage = it "refuses to run a project no language recognises" $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            -- Given
            params <- defaultParams (encodeOsPathString tmpFp)
            logsRef <- newIORef (TestLogs [])

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                    . runFakeD2 D2Missing
                    . runReportProblem
                    . runConcurrent
                    $ doWork params {command = CheckC}

            -- Then
            res `shouldBe` Left (NoFrontendDetected (ProjectRoot params.projectPath) ["TypeScript (tsconfig.json)", "Haskell (*.cabal)"])
            logs <- readIORef logsRef
            -- The title and the warning every run without Rulebooks gives, but
            -- no claim to have detected a language.
            fmap fst logs.transcript `shouldBe` [Title, Warning]

    -- A path that is not a directory holds no tsconfig.json: the person is told
    -- no language matched, not shown a raw IO exception.
    itFindsNoLanguageInMissingPath = it "refuses to run a project path that does not exist" $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            -- Given
            params <- defaultParams (encodeOsPathString $ tmpFp <> "/missing")

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runMockCLI defaultMockCLI
                    . runFakeD2 D2Missing
                    . runReportProblem
                    . runConcurrent
                    $ doWork params {command = CheckC}

            -- Then
            res `shouldBe` Left (NoFrontendDetected (ProjectRoot params.projectPath) ["TypeScript (tsconfig.json)", "Haskell (*.cabal)"])

    -- Detecting the language must not reorder what a broken project hears
    -- first: its Rulebooks are read before any language is required.
    itReportsRulebookBeforeLanguage project =
        it ("reports the broken rulebooks of " <> project <> " before finding no language") $
            withSystemTempDirectory "deslop-test" $ \tmpFp -> do
                -- Given
                let tmpDir = encodeOsPathString tmpFp
                copyDir (fixturesPath </> encodeOsPathString project) tmpDir
                removeFile (tmpFp <> "/tsconfig.json")
                params <- defaultParams tmpDir

                -- When
                res <-
                    runEff
                        . runFileSystemIO
                        . runErrorNoCallStack @DeslopError
                        . runMockCLI defaultMockCLI
                        . runFakeD2 D2Missing
                        . runReportProblem
                        . runConcurrent
                        $ doWork params {command = CheckC}

                -- Then
                res `shouldSatisfy` \case
                    Left (RulebookError _) -> True
                    _ -> False

    -- Deslop's own layering (deslop/rules/), judged by its own Haskell
    -- frontend. Asserted clean rather than goldened: a golden would change
    -- with every module the repository gains.
    itChecksItself = it "checks its own repository clean" $ do
        -- Given
        defParams <- defaultParams [osp|.|]
        filesRef <- newIORef Nothing
        logsRef <- newIORef (TestLogs [])

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                . runFakeD2 D2Missing
                . runReportProblem
                . runConcurrent
                $ doWork defParams {command = CheckC}

        -- Then
        logs <- readIORef logsRef
        (renderTranscript logs, fmap (.verdict) res) `shouldSatisfy` ((== Right Clean) . snd)

    -- The diagram a fixture commits under deslop/diagram is the one this run
    -- writes, so the example a reader opens cannot drift from what the
    -- command produces. d2 itself is faked: rendering is its job, not ours.
    itDiagrams project = it ("diagrams " <> project) $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            -- Given: named after the fixture, which the transcript's title quotes
            let tmpDir = encodeOsPathString tmpFp </> encodeOsPathString project
            let committed = fixturesPath </> encodeOsPathString project </> diagramSource
            copyDir (fixturesPath </> encodeOsPathString project) tmpDir
            params <- defaultParams tmpDir
            logsRef <- newIORef (TestLogs [])

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                    . runFakeD2 D2Installed
                    . runReportProblem
                    . runConcurrent
                    $ doWork params {command = DiagramC}

            -- Then
            fmap (.verdict) res `shouldBe` Right Clean
            written <- SFO.readFile' $ tmpDir </> diagramSource
            expected <- SFO.readFile' committed
            written `shouldBe` expected
            logs <- readIORef logsRef
            pure . defaultGolden ("diagram-" <> project) . T.unpack $
                renderTranscript logs <> renderResult res

    -- Without d2 there is no diagram, and nothing half-written either: the
    -- run stops before it has read the project.
    itRefusesToDiagramWithoutD2 project = it ("refuses to diagram " <> project <> " without d2") $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            -- Given
            let tmpDir = encodeOsPathString tmpFp
            copyDir (fixturesPath </> encodeOsPathString project) tmpDir
            params <- defaultParams tmpDir
            logsRef <- newIORef (TestLogs [])

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                    . runFakeD2 D2Missing
                    . runReportProblem
                    . runConcurrent
                    $ doWork params {command = DiagramC}

            -- Then
            first humanReadable res `shouldBe` Left "d2 not found on PATH. Install it: https://d2lang.com/tour/install"
            SDO.doesDirectoryExist (tmpDir </> [osp|deslop/diagram|]) `shouldReturn` False
            logs <- readIORef logsRef
            logs.transcript `shouldBe` []

    diagramSource = [osp|deslop/diagram/overview.d2|]

    itChecks project = it ("checks " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef (TestLogs [])
        defParams <- defaultParams projectPath
        let params = defParams {command = CheckC}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                . runFakeD2 D2Missing
                . runReportProblem
                . runConcurrent
                $ doWork params

        -- Then
        written <- readIORef filesRef
        written `shouldBe` Nothing
        logs <- readIORef logsRef
        pure . defaultGolden ("check-" <> project) . T.unpack $
            renderTranscript logs <> renderResult res

    itBaselines project = it ("baselines " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef (TestLogs [])
        defParams <- defaultParams projectPath
        let params = defParams {command = BaselineC}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                . runFakeD2 D2Missing
                . runReportProblem
                . runConcurrent
                $ doWork params

        -- Then
        fmap (.verdict) res `shouldBe` Right Clean
        content <- requireJust "Expected baseline.yaml to be written" =<< readIORef filesRef
        traverse_ (`shouldSatisfy` namesNoMachine params.projectPath)
            . baselineEntries
            $ content
        logs <- readIORef logsRef
        pure . defaultGolden ("baseline-" <> project) . T.unpack $
            renderTranscript logs
                <> renderResult res
                <> "\n>>> baseline.yaml\n"
                <> TE.decodeUtf8 content

    itFixes project filesToCheck = it ("fixes " <> project) $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            let tmpDir = encodeOsPathString tmpFp
            -- Given
            let projectPath = fixturesPath </> encodeOsPathString project
            copyDir projectPath tmpDir
            params <- defaultParams tmpDir
            logsRef <- newIORef (TestLogs [])

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runMockCLI defaultMockCLI {logsRef = Just logsRef}
                    . runFakeD2 D2Missing
                    . runReportProblem
                    . runConcurrent
                    $ doWork params

            -- Then
            fmap (.verdict) res `shouldBe` Right Clean
            logs <- readIORef logsRef
            let modified = [msg | (Change, msg) <- logs.transcript]
            -- Files are repaired concurrently; the changelog must not say so.
            modified `shouldBe` sort modified
            fullSnapshot <- snapshot tmpDir filesToCheck
            return . defaultGolden ("fix-" <> project) $
                fullSnapshot <> "\n>>> changelog\n" <> T.unpack (T.unlines modified)
