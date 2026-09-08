module E2E.ProjectGoldenSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop (doWork)
import Deslop.Error (DeslopError (..))
import Deslop.RunReport (RunReport (..), Verdict (..))
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, renderTranscript, runMockCLI)
import Doubles.FileSystem (runMockWrFileSystem)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
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
import Git.Ignore (loadGitIgnore)
import Params (Command (..), Params (..))
import System.OsPath (OsPath, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (copyDir, fixturesPath, mkAbsolute, requireJust, snapshot)
import TypeScript.Iterator (getTsFiles)
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

    itFailsToLoadRulebook "ts-invalid-rulebook-project"
    itFailsToLoadTsConfig "ts-broken-extends-project"

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

    itFixes
        "ts-barrel-project"
        [ "src/lib/index.ts"
        , "src/lib/literals.ts" -- statement-shaped text, must not be rewritten
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
                loadGitIgnore absProjectPath >>= (`getTsFiles` absProjectPath)

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

    -- A tsconfig Deslop cannot resolve - here an `extends` naming a file that
    -- is not there - must abort the run before any file is checked, naming the
    -- missing file and the chain that reached it.
    itFailsToLoadTsConfig project = it ("refuses to run " <> project) $ do
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
                . runReportProblem
                . runConcurrent
                $ doWork params

        -- Then
        res `shouldSatisfy` isLeft
        written <- readIORef filesRef
        written `shouldBe` Nothing
        logs <- readIORef logsRef
        pure . defaultGolden ("tsconfig-error-" <> project) . T.unpack $
            renderTranscript logs <> renderResult res

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

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runMockCLI defaultMockCLI
                    . runReportProblem
                    . runConcurrent
                    $ doWork params

            -- Then
            fmap (.verdict) res `shouldBe` Right Clean
            fullSnapshot <- snapshot tmpDir filesToCheck
            return $ defaultGolden ("fix-" <> project) fullSnapshot
