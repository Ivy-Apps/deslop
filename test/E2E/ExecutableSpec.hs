{- | The real @deslop@ executable, run as a subprocess: what a person or a CI
job actually sees - the exit code, and where each message lands.

The in-process goldens of "E2E.ProjectGoldenSpec" pin what a run says. These
pin only what they cannot: the process around it - the command line, and the
exit code a CI job gates on. Kept to a handful, because each spawns a process.
-}
module E2E.ExecutableSpec (spec) where

import Data.Text qualified as T
import Data.Version (showVersion)
import FileSystem.Path (decodeOsPath, encodeOsPathString)
import Fixtures.OnDisk (Language (..), projectIn)
import Paths_deslop (version)
import System.Exit (ExitCode (..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import TestUtils (copyDir)
import UnliftIO.Temporary (withSystemTempDirectory)

-- | How the process exited, and what it wrote to stdout and to stderr.
data Ran = Ran
    { exitCode :: ExitCode
    , out :: Text
    , err :: Text
    }

deslop :: [String] -> IO Ran
deslop args = do
    (exitCode, out, err) <- readProcessWithExitCode "deslop" args ""
    pure Ran {exitCode, out = toText out, err = toText err}

pathOf :: OsPath -> String
pathOf = toString . decodeOsPath

spec :: Spec
spec = describe "E2E.Executable" $ do
    describe "a run" $ do
        it "exits non-zero when check finds problems" $ do
            ran <- deslop ["check", pathOf (projectIn TypeScript "ts-cycles-project")]

            ran.exitCode `shouldBe` ExitFailure 1
            ran.err `shouldSatisfy` T.isInfixOf "❌ Error: Found 1 problem, none auto-fixable."

        it "exits zero when check finds nothing outside the Baseline" $
            withSystemTempDirectory "deslop-exe" $ \tmp -> do
                copyDir (projectIn TypeScript "ts-cycles-project") (encodeOsPathString tmp)
                baselined <- deslop ["baseline", tmp]

                ran <- deslop ["check", tmp]

                (baselined.exitCode, ran.exitCode) `shouldBe` (ExitSuccess, ExitSuccess)
                ran.out `shouldSatisfy` T.isInfixOf "✅ Success: No problems found."

        it "exits non-zero, saying why, for a project path that does not exist" $ do
            ran <- deslop ["check", "/nonexistent/deslop-project"]

            ran.exitCode `shouldBe` ExitFailure 1
            ran.err `shouldSatisfy` T.isInfixOf "❌ Error: No supported project found in /nonexistent/deslop-project."

        it "exits non-zero, saying why, for a rulebook it cannot load" $ do
            ran <- deslop ["check", pathOf (projectIn TypeScript "ts-invalid-rulebook-project")]

            ran.exitCode `shouldBe` ExitFailure 1
            ran.err `shouldSatisfy` T.isInfixOf "❌ Error: Could not load 4 rulebooks."

    describe "the command line" $ do
        it "prints the version" $ do
            ran <- deslop ["--version"]

            (ran.exitCode, ran.out) `shouldBe` (ExitSuccess, "Deslop Version " <> toText (showVersion version) <> "\n")

        it "prints the help" $ do
            ran <- deslop ["--help"]

            ran.exitCode `shouldBe` ExitSuccess
            ran.out `shouldSatisfy` T.isPrefixOf "Deslop - A Haskell-powered code cleaner ✨\n\nUsage: deslop [-v|--version] COMMAND [PROJECT_DIR]"

        it "refuses an unknown command, naming the ones it knows" $ do
            ran <- deslop ["lint"]

            ran.exitCode `shouldBe` ExitFailure 1
            ran.err `shouldSatisfy` T.isPrefixOf "Unknown command 'lint'. Expected: check, fix, or baseline\n"

        it "shows the help, failing, when given nothing to do" $ do
            ran <- deslop []

            ran.exitCode `shouldBe` ExitFailure 1
            ran.err `shouldSatisfy` T.isPrefixOf "Deslop - A Haskell-powered code cleaner ✨"
