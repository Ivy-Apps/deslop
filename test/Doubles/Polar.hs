module Doubles.Polar (
    MockPolar (..),
    runMockPolar,
) where

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects.Polar (LicenseError, LicenseKey, Polar (..))

data MockPolar = MockPolar
    { checkLicense :: LicenseKey -> Either LicenseError ()
    }

runMockPolar :: MockPolar -> Eff (Polar : es) a -> Eff es a
runMockPolar mock = interpret $ \_ -> \case
    CheckLicense key -> pure $ mock.checkLicense key
