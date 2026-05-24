module Types (
    DeslopError (..),
    Renderable (..),
    deslopLicenseEnv,
) where

import System.OsPath (OsPath)

class Renderable a where
    render :: a -> Text

instance (Renderable a) => Renderable [a] where
    render = foldl' (\acc x -> acc <> render x) ""

deslopLicenseEnv :: Text
deslopLicenseEnv = "DESLOP_LICENSE_KEY"

data DeslopError
    = TsConfigNotFoundError OsPath
    | TsConfigParseError Text
    | CheckModeFoundProblems
    | RulebookErorr Text
    | InvalidRuleConfig Text
    | CaptchaError
    | NoLicenseError
    | InvalidLicenseError
    | UsageExceededError
    | LicenseGenericError
    deriving (Show, Eq)
