module E2E.ProjectGoldenSpec (spec) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (fromJust, isNothing)
import Data.Text qualified as T
import Deslop (deslopProject, doWork)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import Effects.ReportProblem (runReportProblem)
import Params
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (TestLogs (..), copyDir, defaultParams, projectFixturePath, runAIAlwaysFail, runCLILogTest, runFileSystemTest, runGitTest, snapshot, testSecrets)
import Types (DeslopError (CheckModeFoundProblems))
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "Whole Project Golden Tests" $ do
    it "correctly transforms ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir
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
            let filesToVerify =
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
            fullSnapshot <- snapshot tmpDir filesToVerify
            return $ defaultGolden "ts-project-1-snapshot" fullSnapshot

    it "checks ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir
            filesRef <- newIORef Nothing
            logsRef <- newIORef Nothing
            let params = (defaultParams tmpDir) {checkMode = True}

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
            -- Removes the tmp dir path from the log so the golden test is stable
            let problemsLogNormalized = T.unpack . T.replace (T.pack tmpDir) "" $ logs.problems
            return $ defaultGolden "ts-project-1-problem-logs" problemsLogNormalized
