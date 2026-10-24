module Effects.FileSystemSpec (spec) where

import Effectful (runEff)
import Effects.FileSystem (fsListDirectory, fsMkAbsolute, runRoFileSystemIO)
import FileSystem.Path (AbsPath (..))
import System.Directory.OsPath qualified as SDO
import System.OsPath (isAbsolute, osp)
import System.OsString qualified as OS
import Fixtures.OnDisk (staticTree)
import Test.Hspec

spec :: Spec
spec = describe "Effects.FileSystem" $ do
    it "fsMkAbsolute" $ do
        -- Given
        let path = [osp|./test/../deslop.cabal|]

        -- When
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- Then
        OS.length absPath.osPath `shouldSatisfy` (> OS.length path)
        absPath.osPath `shouldSatisfy` isAbsolute
        SDO.doesFileExist absPath.osPath `shouldReturn` True

    it "fsListDirectory" $ do
        -- Given
        let path = staticTree
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- When
        entries <- runEff . runRoFileSystemIO $ fsListDirectory absPath

        -- Then
        entries `shouldSatisfy` all (isAbsolute . (.osPath))
        -- Sorted, not merely listed: callers turn this order into output a
        -- user reads, and the filesystem's own order differs between machines.
        entries `shouldBe` sort entries
