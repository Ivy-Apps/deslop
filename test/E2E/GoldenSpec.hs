module E2E.GoldenSpec (spec) where

import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef)
import Effectful (runEff)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Hspec

import Deslop (deslopFile)
import TestUtils (runFileSystemTest)

spec :: Spec
spec = describe "E2E Golden Tests" $ do
  inputFiles <- runIO $ listFixtures "test/fixtures/typescript/input"

  mapM_ createGoldenTest inputFiles
 where
  createGoldenTest :: FilePath -> Spec
  createGoldenTest filename = do
    let testName = takeBaseName filename

    it ("removes AI slop from " ++ testName) $ do
      -- Given
      let inputPath = "test/fixtures/typescript/input" </> filename
      let expectedPath = "test/fixtures/typescript/expected" </> filename
      captureRef <- newIORef Nothing

      -- When
      runEff $ runFileSystemTest captureRef $ do
        deslopFile inputPath "_ignored.ts"

      -- Then
      actualResult <- readIORef captureRef
      case actualResult of
        Nothing -> fail "The program did not write any output!"
        Just actualBytes -> do
          hasExpected <- doesFileExist expectedPath
          if not hasExpected
            then fail $ "Missing expected file: " ++ expectedPath
            else do
              expectedBytes <- BS.readFile expectedPath
              actualBytes `shouldBe` expectedBytes

listFixtures :: FilePath -> IO [FilePath]
listFixtures dir = do
  files <- listDirectory dir
  return $ filter (\f -> ".ts" `isSuffixOf` f) files
 where
  isSuffixOf s f = (take (length s) (reverse f)) == (reverse s)