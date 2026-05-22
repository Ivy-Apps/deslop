module Effects.System (
    System (..),
    sLookupEnv,
    sIsTerminal,
    runSystem,
) where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Posix (queryTerminal, stdError, stdInput, stdOutput)

data System :: Effect where
    LookupEnv :: String -> System es (Maybe String)
    IsTerminal :: System es Bool

type instance DispatchOf System = 'Dynamic

sLookupEnv :: (System :> es) => String -> Eff es (Maybe String)
sLookupEnv = send . LookupEnv

sIsTerminal :: (System :> es) => Eff es Bool
sIsTerminal = send IsTerminal

runSystem :: (IOE :> es) => Eff (System : es) a -> Eff es a
runSystem = interpret $ \_ -> \case
    LookupEnv key -> liftIO $ lookupEnv key
    IsTerminal ->
        liftIO $ allM queryTerminal [stdInput, stdOutput, stdError]
