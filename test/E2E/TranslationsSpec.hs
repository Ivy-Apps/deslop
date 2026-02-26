module E2E.TranslationsSpec (spec) where

import Data.IORef (newIORef)
import Data.IORef.Extra (readIORef)
import Deslop
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils
import Types
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "NextJS Translations" $ do
    it "translates ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir
            logsRef <- newIORef Nothing

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @TranslationsError
                    . runCLILogTest logsRef
                    . runAITest
                    . runConcurrent
                    $ translateProject (defaultParams tmpDir)

            -- Then
            res `shouldBe` Right ()
            logs <- readIORef logsRef
            logs `shouldBe` Nothing
            let filesToVerify =
                    [ "messages/es.json"
                    , "messages/fr.json"
                    , "messages/en.json"
                    ]
            fullSnapshot <- snapshot tmpDir filesToVerify
            return $ defaultGolden "translations-1" fullSnapshot

    it "missing translations folder" $ do
        -- Given
        logsRef <- newIORef Nothing

        -- When
        res <-
            runEff
                . runFileSystemIO
                . runErrorNoCallStack @TranslationsError
                . runCLILogTest logsRef
                . runAIAlwaysFail
                . runConcurrent
                $ translateProject (defaultParams "invalid-dir")

        -- Then
        res `shouldBe` Left MessagesNotFound
