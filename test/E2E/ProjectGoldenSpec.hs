module E2E.ProjectGoldenSpec (spec) where

import Control.Monad (forM, forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Deslop (DeslopError (..), Params (..), deslopProject)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
 )
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (defaultParams, runCLILogTest, runGitTest)
import UnliftIO.Temporary (withSystemTempDirectory)

projectFixturePath :: FilePath
projectFixturePath = "test/fixtures/ts-project-1"

spec :: Spec
spec = describe "Whole Project Golden Tests" $ do
    it "correctly transforms ts-project-1" $ do
        -- Setup Safe Temp Directory
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
            results <- forM filesToVerify $ \relPath -> do
                content <- TIO.readFile (tmpDir </> relPath)
                let header = "\n\n\n>>> FILE: " <> T.pack relPath <> "\n"
                return $ header <> content
            let fullSnapshot = T.dropWhile (== '\n') $ T.concat results
            return $ defaultGolden "ts-project-1-snapshot" (T.unpack fullSnapshot)

    it "transforms only modified files" $ do
        -- Setup Safe Temp Directory
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
                        [ tmpDir </> "src/app/[locale]/login/page.tsx"
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
            results <- forM filesToVerify $ \relPath -> do
                content <- TIO.readFile (tmpDir </> relPath)
                let header = "\n\n\n>>> FILE: " <> T.pack relPath <> "\n"
                return $ header <> content
            let fullSnapshot = T.dropWhile (== '\n') $ T.concat results
            return $ defaultGolden "ts-project-1-git-modified" (T.unpack fullSnapshot)

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
    createDirectoryIfMissing True dst
    content <- listDirectory src
    forM_ content $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDir srcPath dstPath
            else copyFile srcPath dstPath