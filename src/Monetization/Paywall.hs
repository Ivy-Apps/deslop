module Monetization.Paywall (paywallCheck) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effects.CLI (CLI, logWarning)
import Effects.Polar (LicenseError (..), LicenseKey (LicenseKey), Polar, pCheckLicense)
import Effects.Random (Random)
import Effects.System (System, sLookupEnv)
import Monetization.CIDetection (DetectedEnv (..), detectEnv)
import Monetization.Captcha (triggerCaptcha)
import Types (DeslopError, deslopLicenseEnv)
import Types qualified as TP

paywallCheck ::
    ( System :> es
    , Random :> es
    , CLI :> es
    , Polar :> es
    , Error DeslopError :> es
    ) =>
    Eff es ()
paywallCheck = do
    env <- detectEnv
    case env of
        CI -> do
            res <- checkLicense
            case res of
                Nothing -> throwError TP.NoLicenseError
                Just (Right ()) -> pure ()
                Just (Left RateLimitError) -> pure ()
                Just (Left InvalidLicenseError) -> throwError TP.InvalidLicenseError
                Just (Left UsageExceededError) -> throwError TP.UsageExceededError
                Just (Left GenericError) -> throwError TP.LicenseGenericError
        MaybeCI -> do
            res <- checkLicense
            case res of
                Just (Right ()) -> pure ()
                Just (Left RateLimitError) -> pure ()
                _ -> do
                    logWarning $
                        "Potential CI environment detected."
                            <> " Using Deslop on the CI requires a license."
                    triggerCaptcha
        Terminal -> pure () -- do nothing, local environment

checkLicense :: (System :> es, Polar :> es) => Eff es (Maybe (Either LicenseError ()))
checkLicense = do
    maybeLicense <- fmap LicenseKey <$> sLookupEnv deslopLicenseEnv
    case maybeLicense of
        Just license -> Just <$> pCheckLicense license
        Nothing -> pure Nothing
