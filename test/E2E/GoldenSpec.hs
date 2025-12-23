module E2E.GoldenSpec (spec) where

import Data.IORef (newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effectful (runEff)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Golden (defaultGolden)

import Deslop (deslopFile)
import TestUtils (runFileSystemTest)

tsFixturesPath :: FilePath
tsFixturesPath = "test/fixtures/typescript"

spec :: Spec
spec = describe "E2E Golden Tests" $ do
  inputFiles <- runIO $ listFixtures tsFixturesPath

  mapM_ createGoldenTest inputFiles
 where
  createGoldenTest :: FilePath -> Spec
  createGoldenTest filename = do
    let testName = takeBaseName filename

    it ("removes AI slop from " ++ testName) $ do
      -- Given
      let inputPath = tsFixturesPath </> filename
      captureRef <- newIORef Nothing

      -- When
      runEff $ runFileSystemTest captureRef $ do
        deslopFile inputPath "_ignored.ts"

      -- Then
      actualResult <- readIORef captureRef
      case actualResult of
        Nothing -> fail "The program did not write any output!"
        Just actualBytes -> do
          let actualContent = T.unpack $ decodeUtf8 actualBytes
          return $ defaultGolden testName actualContent

listFixtures :: FilePath -> IO [FilePath]
listFixtures dir = do
  files <- listDirectory dir
  return $ filter (\f -> takeExtension f == ".ts") files