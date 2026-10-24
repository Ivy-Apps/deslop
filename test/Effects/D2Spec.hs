module Effects.D2Spec (spec) where

import Data.Text qualified as T
import Effectful (runEff)
import Effects.D2 (Availability (..), Rendering (..), d2Availability, d2Render, runD2ProgramIO)
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import TestUtils (ap)
import UnliftIO.Temporary (withSystemTempDirectory)

{- The real d2 is not a test dependency, so each case stands a shell script
in for it: what is under test is how the interpreter reads a program's exit
code and stderr, and what it passes the program.
-}
spec :: Spec
spec = describe "Effects.D2" $ do
    describe "d2Availability" $ do
        it "is Installed when d2 --version succeeds" $ withFakeD2 "exit 0" $ \d2 -> do
            runEff (runD2ProgramIO d2 d2Availability) `shouldReturn` Installed

        it "is NotInstalled when d2 --version fails" $ withFakeD2 "exit 1" $ \d2 -> do
            runEff (runD2ProgramIO d2 d2Availability) `shouldReturn` NotInstalled

        it "is NotInstalled when there is no program to run" $
            withSystemTempDirectory "deslop-d2" $ \dir -> do
                runEff (runD2ProgramIO (dir </> "d2") d2Availability) `shouldReturn` NotInstalled

    describe "d2Render" $ do
        it "renders the source to the output with ELK" $ withFakeD2 "echo \"$@\" > \"$(dirname \"$0\")/args\"" $ \d2 -> do
            -- When
            rendering <- runEff . runD2ProgramIO d2 $ d2Render (ap "/p/overview.d2") (ap "/p/overview.svg")

            -- Then
            rendering `shouldBe` Rendered
            args <- T.words . decodeUtf8 <$> readFileBS (takeDirectory d2 </> "args")
            args `shouldContain` ["--layout", "elk"]
            drop (length args - 2) args `shouldBe` ["/p/overview.d2", "/p/overview.svg"]

        it "keeps what d2 said when it could not render" $ withFakeD2 "echo '  err: bad edge  ' >&2; exit 1" $ \d2 -> do
            runEff (runD2ProgramIO d2 $ d2Render (ap "/p/overview.d2") (ap "/p/overview.svg"))
                `shouldReturn` RenderFailed "err: bad edge"

-- | Runs the action with an executable shell script, the given body, standing in for d2.
withFakeD2 :: Text -> (FilePath -> IO a) -> IO a
withFakeD2 body action = withSystemTempDirectory "deslop-d2" $ \dir -> do
    let d2 = dir </> "d2"
    writeFileText d2 $ "#!/bin/sh\n" <> body <> "\n"
    setPermissions d2 . setOwnerExecutable True =<< getPermissions d2
    action d2
