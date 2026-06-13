module Effects.System (
    System (..),
    sLookupEnv,
    runSystem,
) where

import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

data System :: Effect where
    LookupEnv :: Text -> System es (Maybe Text)

type instance DispatchOf System = 'Dynamic

sLookupEnv :: (System :> es) => Text -> Eff es (Maybe Text)
sLookupEnv = send . LookupEnv

runSystem :: (IOE :> es) => Eff (System : es) a -> Eff es a
runSystem = interpret $ \_ -> \case
    LookupEnv key -> liftIO . (fmap . fmap) T.pack . lookupEnv . T.unpack $ key
