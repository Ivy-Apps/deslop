module Types (
    DeslopError (..),
    TranslationsError (..),
    Renderable (..),
) where

import Data.Text (Text)

class Renderable a where
    render :: a -> Text

instance (Renderable a) => Renderable [a] where
    render = foldl' (\acc x -> acc <> render x) ""

data DeslopError
    = TsConfigNotFoundError FilePath
    | TsConfigParseError FilePath
    | CheckModeFoundProblems
    deriving (Show, Eq)

data TranslationsError
    = MessagesNotFound
    | ParseTranslationsError
    | TranslateError Text
    deriving (Show, Eq)
