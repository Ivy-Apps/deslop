{- | What a person or a CI job sees around a run: the exit code, and what the
command line itself answers.

The in-process goldens of "E2E.ProjectGoldenSpec" pin what a run says. These
pin only what they cannot: the exit code 'deslop' returns, the reason it gives
when it fails (logged as an 'Error', which is what goes to stderr), and what
the command line prints before any run starts.

They stop at the 'ExitCode' and the 'LogStyle'. The thin step from there to
the process, the 'exitWith' in 'runDeslop' and 'runCLI' picking a stream per
style, is deliberately not run, so the suite never needs the executable built
(GH #249).
-}
module E2E.CommandLineSpec (spec) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Deslop (deslop)
import Deslop.Command (Command (..))
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, runMockCLI)
import Doubles.Git (noChanges, runGitInMemory)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effects.CLI (LogStyle (..))
import Effects.FileSystem (runFileSystemIO)
import Fixtures.OnDisk (Language (..), inCopyOf, projectIn)
import Options.Applicative (ParserResult (Failure), execParserPure, renderFailure)
import Params (ParamsDto (..), paramsParser, parserPrefs)
import Paths_deslop (version)
import System.Exit (ExitCode (..))
import System.OsPath (OsPath, osp)
import Test.Hspec

-- | How a run ended, and every message it logged.
data Ran = Ran
    { exitCode :: ExitCode
    , logs :: TestLogs
    }

run :: Command -> OsPath -> IO Ran
run command projectPath = do
    logsRef <- newIORef (TestLogs [])
    exitCode <-
        runEff
            . runFileSystemIO
            . runMockCLI defaultMockCLI {logsRef = Just logsRef}
            . runConcurrent
            . runGitInMemory noChanges
            . deslop
            $ ParamsDto {command, projectPath}
    Ran exitCode <$> readIORef logsRef

-- | The messages a run logged in the given style, in order.
loggedAs :: LogStyle -> Ran -> [Text]
loggedAs style ran = [msg | (s, msg) <- ran.logs.transcript, s == style]

{- | What the command line answers for arguments it does not turn into a run:
the text it prints and the exit code. The text goes to stdout when the code
is 'ExitSuccess' and to stderr otherwise.
-}
answer :: [String] -> IO (ExitCode, Text)
answer args = case execParserPure parserPrefs paramsParser args of
    Failure failure -> pure . fmap toText . swap $ renderFailure failure "deslop"
    _ranInstead -> fail $ "the command line parsed " <> show args <> " into a run"

spec :: Spec
spec = describe "E2E.CommandLine" $ do
    describe "a run" $ do
        it "exits non-zero when check finds problems" $ do
            ran <- run CheckC (projectIn TypeScript "ts-cycles-project")

            ran.exitCode `shouldBe` ExitFailure 1
            loggedAs Error ran `shouldSatisfy` any (T.isPrefixOf "❌ Error: Found 1 problem, none auto-fixable.")

        it "exits zero when check finds nothing outside the Baseline" $
            inCopyOf TypeScript "ts-cycles-project" $ \tmp -> do
                baselined <- run BaselineC tmp

                ran <- run CheckC tmp

                (baselined.exitCode, ran.exitCode) `shouldBe` (ExitSuccess, ExitSuccess)
                loggedAs Success ran `shouldContain` ["✅ Success: No problems found."]
                loggedAs Error ran `shouldBe` []

        it "exits non-zero, saying why, for a project path that does not exist" $ do
            ran <- run CheckC [osp|/nonexistent/deslop-project|]

            ran.exitCode `shouldBe` ExitFailure 1
            loggedAs Error ran `shouldSatisfy` any (T.isPrefixOf "❌ Error: No supported project found in /nonexistent/deslop-project.")

        it "exits non-zero, saying why, for a rulebook it cannot load" $ do
            ran <- run CheckC (projectIn TypeScript "ts-invalid-rulebook-project")

            ran.exitCode `shouldBe` ExitFailure 1
            loggedAs Error ran `shouldSatisfy` any (T.isPrefixOf "❌ Error: Could not load 4 rulebooks.")

    describe "the command line" $ do
        it "prints the version" $
            answer ["--version"] `shouldReturn` (ExitSuccess, "Deslop Version " <> toText (showVersion version))

        it "prints the help" $ do
            (exitCode, out) <- answer ["--help"]

            exitCode `shouldBe` ExitSuccess
            out `shouldSatisfy` T.isPrefixOf "Deslop - A Haskell-powered code cleaner ✨\n\nUsage: deslop [-v|--version] COMMAND\n"

        it "refuses an unknown command" $ do
            (exitCode, err) <- answer ["lint"]

            exitCode `shouldBe` ExitFailure 1
            err `shouldSatisfy` T.isPrefixOf "Invalid argument `lint'\n"

        it "shows the help, failing, when given nothing to do" $ do
            (exitCode, err) <- answer []

            exitCode `shouldBe` ExitFailure 1
            err `shouldSatisfy` T.isPrefixOf "Deslop - A Haskell-powered code cleaner ✨"
