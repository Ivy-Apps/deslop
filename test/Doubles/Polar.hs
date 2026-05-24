module Doubles.Polar (
    MockPolar (..),
    runMockPolar,
    defaultMockPolar,
) where

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects.Polar (LicenseError, LicenseKey, Polar (..))

data MockPolar = MockPolar
    { checkLicense :: LicenseKey -> Either LicenseError ()
    }

defaultMockPolar :: MockPolar
defaultMockPolar =
    MockPolar
        { checkLicense = \_ -> error "Test error: The Polar mock must not be called."
        }

runMockPolar :: MockPolar -> Eff (Polar : es) a -> Eff es a
runMockPolar mock = interpret $ \_ -> \case
    CheckLicense key -> pure $ mock.checkLicense key
