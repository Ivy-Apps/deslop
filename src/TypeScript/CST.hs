module TypeScript.CST (
    TsProgram (..),
    TsNode (..),
) where

import Effects.FileSystem (AbsPath)
import Types (Renderable (..))

data TsProgram = TsModule
    { path :: AbsPath
    , cst :: [TsNode]
    }
    deriving (Show, Eq)

data TsNode
    = Import
        { prefix :: Text
        , target :: Text
        , suffix :: Text
        }
    | Source
        { raw :: Text
        }
    deriving (Show, Eq)

instance Renderable TsNode where
    render (Source r) = r
    render (Import p t s) = p <> t <> s
