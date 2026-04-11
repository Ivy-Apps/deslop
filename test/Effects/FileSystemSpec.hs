module Effects.FileSystemSpec (spec) where

import Effectful (runEff)
import Effects.FileSystem (AbsPath (..), fsMkAbsolute, runRoFileSystemIO)
import System.Directory.OsPath (doesFileExist)
import System.OsPath (isAbsolute, osp)
import System.OsString qualified as OS
import Test.Hspec

spec :: Spec
spec = describe "FileSystem" $ do
    it "fsMkAbsolute" $ do
        -- Given
        let path = [osp|./test/../deslop.cabal|]

        -- When
        AbsPath absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- Then
        (OS.length absPath) `shouldSatisfy` (> OS.length path)
        absPath `shouldSatisfy` isAbsolute
        doesFileExist absPath `shouldReturn` True
