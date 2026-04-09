module TypeScript.CST (
    TsProgram (..),
    TsNode (..),
) where

import System.OsPath (OsPath)
import Types (Renderable (..))

data TsProgram = TsModule
    { path :: OsPath
    , cst :: [TsNode]
    }
    deriving (Show, Eq)

data TsNode
    = Import
        { prefix :: Text
        , target :: Text
        , suffix :: Text
        }
    | Comment
        { raw :: Text
        , content :: Text
        }
    | Docs
        { raw :: Text
        , content :: Text
        }
    | Source
        { raw :: Text
        }
    deriving (Show, Eq)

instance Renderable TsNode where
    render (Source r) = r
    render (Comment r _) = r
    render (Docs r _) = r
    render (Import p t s) = p <> t <> s
