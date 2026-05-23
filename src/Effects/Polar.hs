module Effects.Polar (
    LicenseKey (..),
    LicenseError (..),
    PolarOrgId (..),
    Polar (..),
    pCheckLicense,
    runPolar,
) where

import Control.Exception (try)
import Data.Aeson (ToJSON)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Network.HTTP.Req (HttpConfig (..), HttpException (..), POST (POST), ReqBodyJson (ReqBodyJson), Scheme (Https), Url, defaultHttpConfig, https, ignoreResponse, req, responseStatusCode, runReq, (/:))

ivyAppsOrgId :: PolarOrgId
ivyAppsOrgId = PolarOrgId "6eef59dc-00eb-4cf9-ba5d-1668063772d7"

baseUrl :: Url 'Https
baseUrl = https "api.polar.sh" /: "v1"

newtype LicenseKey = LicenseKey Text deriving stock (Show, Eq)
newtype PolarOrgId = PolarOrgId Text deriving stock (Show, Eq)
data LicenseError
    = InvalidKeyError
    | UsageExceededError
    | GenericError
    deriving stock (Show, Eq)

data Polar :: Effect where
    CheckLicense :: LicenseKey -> Polar m (Either LicenseError ())

type instance DispatchOf Polar = 'Dynamic

pCheckLicense :: (Polar :> es) => LicenseKey -> Eff es (Either LicenseError ())
pCheckLicense = send . CheckLicense

runPolar :: (IOE :> es) => Eff (Polar : es) a -> Eff es a
runPolar = interpret $ \_ -> \case
    CheckLicense key -> liftIO . sendCheckLincenseReq $ key

--- HTTP ---

data CheckLicenseReqDto = CheckLicenseReqDto
    { key :: Text
    , organization_id :: Text
    , increment_usage :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

sendCheckLincenseReq :: LicenseKey -> IO (Either LicenseError ())
sendCheckLincenseReq (LicenseKey key) = do
    let PolarOrgId orgId = ivyAppsOrgId
    let url = baseUrl /: "customer-portal" /: "license-keys" /: "validate"
    let body =
            CheckLicenseReqDto
                { key = key
                , organization_id = orgId
                , increment_usage = 1
                }
    let customConfig = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
    res <-
        try @HttpException . runReq customConfig $
            req POST url (ReqBodyJson body) ignoreResponse mempty
    case res of
        Right response -> pure $ case responseStatusCode response of
            200 -> Right ()
            404 -> Left InvalidKeyError
            400 -> Left UsageExceededError
            _ -> Left GenericError
        Left _ -> pure $ Left GenericError
