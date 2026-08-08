module Effects.CLI (
    CLI (..),
    LogStyle (..),
    cliLog,
    cliReadLine,
    runCLI,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import UI (blueBold, cyanBold, green, plainOut, redStderr, yellowBold)

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
