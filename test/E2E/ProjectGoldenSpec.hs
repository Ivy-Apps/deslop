module E2E.ProjectGoldenSpec (spec) where

import Data.Maybe (fromJust)
import Data.Text qualified as T
import Deslop (deslopProject, doWork)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import Effects.ReportProblem (runReportProblem)
import FsEncoding (decodePathString, encodePathString)
import Params
import System.OsPath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (TestLogs (..), copyDir, defaultParams, fixturesPath, runAIAlwaysFail, runCLILogTest, runFileSystemTest, runGitTest, snapshot, testSecrets)
import Types (DeslopError (CheckModeFoundProblems))
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "Deslop project" $ do
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
        , "tests/fixtures/fixtures.ts"
        , "vitest.config.ts"
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
        let projectPath = fixturesPath </> encodePathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef Nothing
        let params = (defaultParams projectPath) {checkMode = True}

        -- When
        res <-
            runEff
                . runFileSystemTest filesRef
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
        when (isNothing maybeLogs) $
            expectationFailure "Expected problems to be logged when check mode finds problems"
        let logs = fromJust maybeLogs
        -- Removes the dir path from the log so the golden test is stable
        let problemsLogNormalized =
                T.unpack . T.replace (T.pack (decodePathString projectPath)) "" $ logs.problems
        return $ defaultGolden ("check-" <> project) problemsLogNormalized

    itFixes project filesToCheck = it ("fixes " <> project) $ do
        withSystemTempDirectory "deslop-test" $ \tmpFp -> do
            let tmpDir = encodePathString tmpFp
            -- Given
            let projectPath = fixturesPath </> encodePathString project
            copyDir projectPath tmpDir
            logsRef <- newIORef Nothing

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runCLILogTest logsRef
                    . runGitTest []
                    . runReportProblem
                    . runConcurrent
                    $ deslopProject (defaultParams tmpDir)

            -- Then
            res `shouldBe` Right ()
            logs <- readIORef logsRef
            logs `shouldBe` Nothing
            fullSnapshot <- snapshot tmpDir filesToCheck
            return $ defaultGolden ("fix-" <> project) fullSnapshot
