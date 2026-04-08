module Types (
    DeslopError (..),
    TranslationsError (..),
    Renderable (..),
) where

import Data.Text (Text)
import System.OsPath (OsPath)

class Renderable a where
    render :: a -> Text

instance (Renderable a) => Renderable [a] where
    render = foldl' (\acc x -> acc <> render x) ""

data DeslopError
    = TsConfigNotFoundError OsPath
    | TsConfigParseError OsPath
    | CheckModeFoundProblems
    deriving (Show, Eq)

data TranslationsError
    = MessagesNotFound
    | ParseTranslationsError
    | TranslateError Text
    deriving (Show, Eq)
