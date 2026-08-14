module E2E.ProjectGoldenSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop (doWork)
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, renderTranscript, runMockCLI)
import Doubles.FileSystem (runMockWrFileSystem)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (RelativePath (osPath), decodeOsPath, encodeOsPathString, relativePathTo, runFileSystemIO, runRoFileSystemIO)
import Effects.ReportProblem (runReportProblem)
import Git.Ignore (loadGitIgnore)
import Params (Command (..), Params (..))
import System.OsPath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (copyDir, defaultParams, fixturesPath, mkAbsolute, pathSafeGolden, requireJust, snapshot)
import TypeScript.Iterator (getTsFiles)
import Types (DeslopError (..))
import UI (humanReadable)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "E2E.Project" $ do
    itChecks "ts-project-1"
    itChecks "ixartz-next-js-boilerplate"
    itChecks "melzar-nextjs-clean-architecture"
    itChecks "ts-cycles-project"
    itChecks "ts-gitignore-project"
    itChecks "ts-globplus-project"
    itChecks "ts-casing-project"

    itFailsToLoadRulebook "ts-invalid-rulebook-project"

    itBaselines "ts-project-1"
    itBaselines "ixartz-next-js-boilerplate"
    itBaselines "melzar-nextjs-clean-architecture"
    itBaselines "ts-cycles-project"
    itBaselines "ts-gitignore-project"
    itBaselines "ts-globplus-project"
    itBaselines "ts-casing-project"

    itIterates "ts-gitignore-project"

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
  where
    -- The check summary is rendered by runDeslop, outside doWork, so the
    -- transcript alone would not cover it. Appending it goldens the wording
    -- together with the counts that produced it.
    renderResult :: Either DeslopError () -> Text
    renderResult (Right ()) = ""
    renderResult (Left err) = "[exit] " <> humanReadable err <> "\n"

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
            fmap (decodeOsPath . (.osPath) . relativePathTo absProjectPath) files

    -- A rulebook that does not compile must abort the run before any file is
    -- checked, with a message the author can act on. Goldening the transcript
    -- pins the whole path from ruleBookFromDto through to the exit line.
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
        pathSafeGolden ("rulebook-error-" <> project) . T.unpack $
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
        pathSafeGolden ("check-" <> project) . T.unpack $
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
        res `shouldBe` Right ()
        content <- requireJust "Expected baseline.yaml to be written" =<< readIORef filesRef
        logs <- readIORef logsRef
        pathSafeGolden ("baseline-" <> project) . T.unpack $
            renderTranscript logs
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
            res `shouldBe` Right ()
            fullSnapshot <- snapshot tmpDir filesToCheck
            return $ defaultGolden ("fix-" <> project) fullSnapshot
