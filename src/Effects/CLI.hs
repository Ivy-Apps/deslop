{- | Talking to the terminal.

The interpreter below is the only code in Deslop that writes to stdout or
stderr, which is what lets a test run the whole pipeline and read back what it
said. The sentences themselves come from "UI" and are already plain 'Text' by
the time they arrive here; all this adds is a colour and a stream.
-}
module Effects.CLI (
    CLI (..),
    LogStyle (..),
    cliLog,
    cliReadLine,
    runCLI,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Dispatch.Dynamic
import System.Console.ANSI
import System.IO (hPutStr)

-- | How a message is rendered: its colour and which stream it goes to.
data LogStyle
    = -- | Blue bold, stdout
      Title
    | -- | Green, stdout
      Success
    | -- | Yellow bold, stdout
      Warning
    | -- | Red, stderr
      Error
    | -- | Cyan bold, stdout
      Change
    | -- | Uncoloured, stdout
      Plain
    deriving stock (Show, Eq)

data CLI :: Effect where
    Log :: LogStyle -> Text -> CLI m ()
    ReadLine :: CLI m Text

type instance DispatchOf CLI = 'Dynamic

cliLog :: (CLI :> es) => LogStyle -> Text -> Eff es ()
cliLog style = send . Log style

cliReadLine :: (CLI :> es) => Eff es Text
cliReadLine = send ReadLine

runCLI :: (IOE :> es) => Eff (CLI : es) a -> Eff es a
runCLI = interpret $ \_ -> \case
    Log Title msg -> liftIO . blueBold $ msg
    Log Success msg -> liftIO . green $ msg
    Log Warning msg -> liftIO . yellowBold $ msg
    Log Error msg -> liftIO . redStderr $ msg
    Log Change msg -> liftIO . cyanBold $ msg
    Log Plain msg -> liftIO . plainOut $ msg
    ReadLine -> liftIO getLine

--------------------------------------------------------------------------------
-- Colour primitives
--------------------------------------------------------------------------------

blueBold :: Text -> IO ()
blueBold = withSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]

green :: Text -> IO ()
green = withSGR [SetColor Foreground Vivid Green]

yellowBold :: Text -> IO ()
yellowBold = withSGR [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]

cyanBold :: Text -> IO ()
cyanBold = withSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]

plainOut :: Text -> IO ()
plainOut t = TIO.putStrLn t >> hFlush stdout

withSGR :: [SGR] -> Text -> IO ()
withSGR sgr t = do
    setSGR sgr
    TIO.putStrLn t
    setSGR [Reset]
    hFlush stdout

{- | Print to stderr in red. ANSI codes are written raw rather than via 'setSGR',
which only ever targets stdout.
-}
redStderr :: Text -> IO ()
redStderr t = do
    hPutStr stderr redCode
    hPutStr stderr (T.unpack t)
    hPutStr stderr resetCode
    hPutStr stderr "\n"
  where
    redCode = "\x1b[31m"
    resetCode = "\x1b[0m"
