module E2E.ProjectGoldenSpec (spec) where

import Deslop (DeslopError (..), Params (..), deslopProject)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (copyDir, defaultParams, projectFixturePath, runCLILogTest, runGitTest, snapshot)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "Whole Project Golden Tests" $ do
    it "correctly transforms ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir

            -- When
            _ <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runCLILogTest
                    . runGitTest []
                    $ deslopProject (defaultParams tmpDir)

            -- Then
            let filesToVerify =
                    [ "src/app/[locale]/login/page.tsx"
                    , "src/features/home/home-screen.tsx"
                    , "src/features/home/home-component.ts"
                    , "src/features/home/home.spec.ts"
                    , "src/app/[locale]/login/page.tsx"
                    , "src/features/login/login.spec.ts"
                    , "src/features/login/login-form.ts"
                    , "tests/fixtures/fixtures.ts"
                    ]
            fullSnapshot <- snapshot tmpDir filesToVerify
            return $ defaultGolden "ts-project-1-snapshot" fullSnapshot

    it "transforms only modified files" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir

            -- When
            let params = (defaultParams tmpDir) {modified = True}
            _ <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @DeslopError
                    . runCLILogTest
                    . runGitTest
                        [ tmpDir </> "." </> "src/app/[locale]/login/page.tsx"
                        , tmpDir </> "src/features/home/home-screen.tsx"
                        ]
                    $ deslopProject params

            -- Then
            let filesToVerify =
                    [ "src/app/[locale]/login/page.tsx"
                    , "src/features/home/home-screen.tsx"
                    , "src/features/home/home-component.ts"
                    , "src/features/home/home.spec.ts"
                    , "tests/fixtures/fixtures.ts"
                    ]
            fullSnapshot <- snapshot tmpDir filesToVerify
            return $ defaultGolden "ts-project-1-git-modified" fullSnapshot
