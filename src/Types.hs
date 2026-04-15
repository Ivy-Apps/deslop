module Types (
    DeslopError (..),
    Renderable (..),
) where

import System.OsPath (OsPath)

class Renderable a where
    render :: a -> Text

instance (Renderable a) => Renderable [a] where
    render = foldl' (\acc x -> acc <> render x) ""

data DeslopError
    = TsConfigNotFoundError OsPath
    | TsConfigParseError Text
    | CheckModeFoundProblems
    deriving (Show, Eq)
