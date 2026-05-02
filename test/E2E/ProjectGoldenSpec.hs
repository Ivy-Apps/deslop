module E2E.ProjectGoldenSpec (spec) where

import Data.Text qualified as T
import Deslop (doWork)
import Doubles.FileSystem (runMockWrFileSystem)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (encodeOsPathString, runFileSystemIO, runRoFileSystemIO)
import Effects.ReportProblem (runReportProblem)
import Params (Params (..))
import System.OsPath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (TestLogs (..), copyDir, defaultParams, fixturesPath, pathSafeGolden, requireJust, runAIAlwaysFail, runCLILogTest, runGitTest, snapshot, testSecrets)
import Types (DeslopError (CheckModeFoundProblems))
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "E2E.ProjectGolden" $ do
    itChecks "ts-project-1"
    itChecks "ixartz-next-js-boilerplate"
    itChecks "melzar-nextjs-clean-architecture"

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
    itChecks project = it ("checks " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef Nothing
        defParams <- defaultParams projectPath
        let params = defParams {checkMode = True}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runCLILogTest logsRef
                . runGitTest []
                . runReportProblem
                . runAIAlwaysFail
                . runConcurrent
                $ doWork params testSecrets

        -- Then
        res `shouldBe` Left CheckModeFoundProblems
        written <- readIORef filesRef
        written `shouldBe` Nothing
        maybeLogs <- readIORef logsRef

        logs <- requireJust "Expected problems to be logged when check mode finds problems" maybeLogs
        pathSafeGolden ("check-" <> project) (T.unpack logs.problems)

    itFixes project filesToCheck = it ("fixes " <> project) $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            let tmpDir = encodeOsPathString tmpFp
            -- Given
            let projectPath = fixturesPath </> encodeOsPathString project
            copyDir projectPath tmpDir
            logsRef <- newIORef Nothing
            params <- defaultParams tmpDir

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runCLILogTest logsRef
                    . runGitTest []
                    . runReportProblem
                    . runConcurrent
                    . runAIAlwaysFail
                    $ doWork params testSecrets

            -- Then
            res `shouldBe` Right ()
            logs <- readIORef logsRef
            logs `shouldBe` Nothing
            fullSnapshot <- snapshot tmpDir filesToCheck
            return $ defaultGolden ("fix-" <> project) fullSnapshot
