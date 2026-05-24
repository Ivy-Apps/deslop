module Effects.System (
    System (..),
    sLookupEnv,
    sIsTerminal,
    runSystem,
) where

import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Posix (queryTerminal, stdError, stdInput, stdOutput)

data System :: Effect where
    LookupEnv :: Text -> System es (Maybe Text)
    IsTerminal :: System es Bool

type instance DispatchOf System = 'Dynamic

sLookupEnv :: (System :> es) => Text -> Eff es (Maybe Text)
sLookupEnv = send . LookupEnv

sIsTerminal :: (System :> es) => Eff es Bool
sIsTerminal = send IsTerminal

runSystem :: (IOE :> es) => Eff (System : es) a -> Eff es a
runSystem = interpret $ \_ -> \case
    LookupEnv key -> liftIO . (fmap . fmap) T.pack . lookupEnv . T.unpack $ key
    IsTerminal ->
        liftIO $ allM queryTerminal [stdInput, stdOutput, stdError]
