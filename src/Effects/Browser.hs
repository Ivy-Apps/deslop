{- | Showing a written page to the person who asked for it.

Two questions: whether anyone is there to see a page, and opening one in their
default browser. The interpreter asks the CLI whether it writes to a terminal,
reads the environment, and hands the page to the platform's launcher; a test hands "Doubles.Browser" the
answers instead, so no test ever spawns a browser.
-}
module Effects.Browser (
    Attendance (..),
    Browser (..),
    LaunchFailure (..),
    attendanceOf,
    browserAttendance,
    browserOpen,
    runBrowserIO,
) where

import Control.Exception (IOException, try)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effects.CLI (CLI, Stdout (..), cliStdout)
import FileSystem.Path (AbsPath (..), decodeOsPath)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import System.Timeout (timeout)

-- | Whether a person is there to see a page opened for them.
data Attendance = Attended | Unattended
    deriving stock (Show, Eq)

-- | Why a page was not opened. Never fatal: the page is written either way.
data LaunchFailure
    = -- | The platform's launcher (@xdg-open@, @open@, @rundll32@) is not on the PATH, or would not start.
      LauncherMissing Text
    | -- | The launcher ran and gave up, with this exit code.
      LauncherFailed Text Int
    deriving stock (Show, Eq)

data Browser :: Effect where
    AttendanceOf :: Browser m Attendance
    Open :: AbsPath -> Browser m (Either LaunchFailure ())

type instance DispatchOf Browser = 'Dynamic

{- | Whether a person is there: standard output is a terminal and the
environment has no @CI@.
-}
browserAttendance :: (Browser :> es) => Eff es Attendance
browserAttendance = send AttendanceOf

-- | Opens the page in the default browser.
browserOpen :: (Browser :> es) => AbsPath -> Eff es (Either LaunchFailure ())
browserOpen = send . Open

{- | Attended only on a terminal, outside CI. @CI@ counts as set whatever its
value, as CI services set it to @true@ or @1@.
-}
attendanceOf :: Stdout -> Maybe String -> Attendance
attendanceOf ToTerminal Nothing = Attended
attendanceOf ToTerminal (Just _ci) = Unattended
attendanceOf Redirected _ = Unattended

runBrowserIO :: (IOE :> es, CLI :> es) => Eff (Browser : es) a -> Eff es a
runBrowserIO = interpret $ \_ -> \case
    AttendanceOf -> attendanceOf <$> cliStdout <*> lookupEnv "CI"
    Open page -> liftIO . openIO $ decodeOsPath page.osPath

{- | Hands the page to the launcher, then waits 'launchGrace' for its verdict.
A launcher still running by then has started the browser in the foreground,
as @xdg-open@ does without a desktop environment, and is left to it.
-}
openIO :: Text -> IO (Either LaunchFailure ())
openIO page =
    findExecutable launcher >>= \case
        Nothing -> pure . Left $ LauncherMissing name
        Just executable ->
            try @IOException (createProcess (proc executable $ arguments <> [toString page]) {std_in = NoStream, std_out = NoStream, std_err = NoStream})
                >>= \case
                    Left _cannotStart -> pure . Left $ LauncherMissing name
                    Right (_, _, _, process) ->
                        timeout launchGrace (waitForProcess process) <&> \case
                            Just (ExitFailure code) -> Left $ LauncherFailed name code
                            _openedOrStillOpening -> Right ()
  where
    (launcher, arguments) = launcherOn os
    name = toText launcher

{- | The program that opens a file in the default browser, and the arguments
before the file. Windows goes through @rundll32@ rather than @cmd /c start@:
@cmd@ would parse the path as a command line, where an @&@ in a folder name
starts another command.
-}
launcherOn :: String -> (FilePath, [String])
launcherOn = \case
    "darwin" -> ("open", [])
    "mingw32" -> ("rundll32", ["url.dll,FileProtocolHandler"])
    _linuxOrBsd -> ("xdg-open", [])

-- | How long a launcher may take to say it failed: 2s.
launchGrace :: Int
launchGrace = 2000000
