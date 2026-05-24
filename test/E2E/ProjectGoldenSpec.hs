module E2E.ProjectGoldenSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop (doWork)
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, runMockCLI)
import Doubles.FileSystem (runMockWrFileSystem)
import Doubles.Polar (MockPolar (..), defaultMockPolar, runMockPolar)
import Doubles.Random (runMockRandom)
import Doubles.System (MockSystem (mockIsTerminal, mockLookupEnv), defaultMockSystem, runMockSystem)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.CLI (CLI)
import Effects.FileSystem (encodeOsPathString, runFileSystemIO, runRoFileSystemIO)
import Effects.Polar (LicenseKey (..), Polar, runPolar)
import Effects.Polar qualified as Polar
import Effects.Random (Random, runRandom)
import Effects.ReportProblem (runReportProblem)
import Effects.System (System)
import Params (Command (..), Params (..))
import System.OsPath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (copyDir, defaultParams, fixturesPath, pathSafeGolden, requireJust, snapshot)
import Types (DeslopError (..))
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "E2E.Project" $ do
    itChecks "ts-project-1"
    itChecks "ixartz-next-js-boilerplate"
    itChecks "melzar-nextjs-clean-architecture"

    itBaselines "ts-project-1"
    itBaselines "ixartz-next-js-boilerplate"
    itBaselines "melzar-nextjs-clean-architecture"

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

    describe "test paywall" $ do
        it "on CI with valid license should allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockLookupEnv = \case
                                "CI" -> Just "true"
                                "DESLOP_LICENSE_KEY" -> Just "valid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { checkLicense = \case
                                LicenseKey "valid" -> Right ()
                                _ -> error "Invalid Polar test input"
                            }
                    . runMockRandom []
                    . runMockCLI defaultMockCLI
                    $ runPaywallCheckMode
            res `shouldBe` Left CheckModeFoundProblems

        it "on CI with invalid license should block" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockLookupEnv = \case
                                "CI" -> Just "true"
                                "DESLOP_LICENSE_KEY" -> Just "invalid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        defaultMockPolar
                            { checkLicense = \case
                                LicenseKey "invalid" -> Left Polar.InvalidLicenseError
                                _ -> error "Invalid Polar test input"
                            }
                    . runMockRandom []
                    . runMockCLI defaultMockCLI
                    $ runPaywallCheckMode
            res `shouldBe` Left InvalidLicenseError

        it "on MaybeCI with valid license should allow" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "valid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        MockPolar
                            { checkLicense = \case
                                LicenseKey "valid" -> Right ()
                                _ -> error "Invalid Polar test input"
                            }
                    . runMockRandom []
                    . runMockCLI defaultMockCLI
                    $ runPaywallCheckMode
            res `shouldBe` Left CheckModeFoundProblems

        it "on MaybeCI with invalid license should captcha and fail" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "invalid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        MockPolar
                            { checkLicense = \case
                                LicenseKey "invalid" -> Left Polar.InvalidLicenseError
                                _ -> error "Invalid Polar test input"
                            }
                    . runMockRandom [0, 2, 2]
                    . runMockCLI defaultMockCLI {readLines = ["incorrect"]}
                    $ runPaywallCheckMode
            res `shouldBe` Left CaptchaError

        it "on MaybeCI with invalid license should captcha and succeed" $ do
            res <-
                runEff
                    . runMockSystem
                        defaultMockSystem
                            { mockIsTerminal = False
                            , mockLookupEnv = \case
                                "DESLOP_LICENSE_KEY" -> Just "invalid"
                                _ -> Nothing
                            }
                    . runMockPolar
                        MockPolar
                            { checkLicense = \case
                                LicenseKey "invalid" -> Left Polar.InvalidLicenseError
                                _ -> error "Invalid Polar test input"
                            }
                    . runMockRandom [0, 2, 2]
                    . runMockCLI defaultMockCLI {readLines = ["4"]}
                    $ runPaywallCheckMode
            res `shouldBe` Left CheckModeFoundProblems
  where
    itChecks project = it ("checks " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef Nothing
        defParams <- defaultParams projectPath
        let params = defParams {command = CheckC}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {problemsRef = Just logsRef}
                . runReportProblem
                . runConcurrent
                . runMockSystem defaultMockSystem {mockIsTerminal = True}
                . runRandom
                . runMockPolar defaultMockPolar
                $ doWork params

        -- Then
        res `shouldBe` Left CheckModeFoundProblems
        written <- readIORef filesRef
        written `shouldBe` Nothing
        maybeLogs <- readIORef logsRef

        logs <- requireJust "Expected problems to be logged when check mode finds problems" maybeLogs
        pathSafeGolden ("check-" <> project) (T.unpack logs.problems)

    itBaselines project = it ("baselines " <> project) $ do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString project
        filesRef <- newIORef Nothing
        logsRef <- newIORef Nothing
        defParams <- defaultParams projectPath
        let params = defParams {command = BaselineC}

        -- When
        res <-
            runEff
                . runMockWrFileSystem filesRef
                . runRoFileSystemIO
                . runErrorNoCallStack @DeslopError
                . runMockCLI defaultMockCLI {problemsRef = Just logsRef}
                . runReportProblem
                . runConcurrent
                . runMockSystem defaultMockSystem {mockIsTerminal = True}
                . runRandom
                . runPolar
                $ doWork params

        -- Then
        res `shouldBe` Right ()
        content <- requireJust "Expected baseline.yaml to be written" =<< readIORef filesRef
        pathSafeGolden ("baseline-" <> project) (T.unpack . TE.decodeUtf8 $ content)

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
                    . runMockCLI defaultMockCLI {problemsRef = Just logsRef}
                    . runReportProblem
                    . runConcurrent
                    . runMockSystem defaultMockSystem {mockIsTerminal = True}
                    . runRandom
                    . runPolar
                    $ doWork params

            -- Then
            res `shouldBe` Right ()
            logs <- readIORef logsRef
            logs `shouldBe` Nothing
            fullSnapshot <- snapshot tmpDir filesToCheck
            return $ defaultGolden ("fix-" <> project) fullSnapshot

    runPaywallCheckMode ::
        ( IOE :> es
        , System :> es
        , Random :> es
        , Polar :> es
        , CLI :> es
        ) =>
        Eff es (Either DeslopError ())
    runPaywallCheckMode = do
        -- Given
        let projectPath = fixturesPath </> encodeOsPathString "ts-project-1"
        filesRef <- liftIO $ newIORef Nothing
        defParams <- liftIO $ defaultParams projectPath
        let params = defParams {command = CheckC}

        -- When
        runMockWrFileSystem filesRef
            . runRoFileSystemIO
            . runErrorNoCallStack @DeslopError
            . runReportProblem
            . runConcurrent
            $ doWork params
