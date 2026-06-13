module Doubles.System (
    MockSystem (..),
    defaultMockSystem,
    runMockSystem,
) where

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects.System (System (..))

data MockSystem es = MockSystem
    { mockLookupEnv :: Text -> Maybe Text
    }

defaultMockSystem :: MockSystem es
defaultMockSystem =
    MockSystem
        { mockLookupEnv = const Nothing
        }

runMockSystem :: MockSystem es -> Eff (System : es) a -> Eff es a
runMockSystem mocks = interpret $ \_env -> \case
    LookupEnv key -> pure $ mocks.mockLookupEnv key
