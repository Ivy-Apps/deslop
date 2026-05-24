module Doubles.System (
    MockSystem (..),
    defaultMockSystem,
    runMockSystem,
) where

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects.System (System (..))

data MockSystem es = MockSystem
    { mockLookupEnv :: Text -> Eff es (Maybe Text)
    , mockIsTerminal :: Eff es Bool
    }

defaultMockSystem :: MockSystem es
defaultMockSystem =
    MockSystem
        { mockLookupEnv = const $ pure Nothing
        , mockIsTerminal = pure False
        }

runMockSystem :: MockSystem es -> Eff (System : es) a -> Eff es a
runMockSystem mocks = interpret $ \_env -> \case
    LookupEnv key -> mocks.mockLookupEnv key
    IsTerminal -> mocks.mockIsTerminal
