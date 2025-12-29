module TypeScript.AST where

import Data.Text (Text)

data TsProgram = TsModule
    { path :: FilePath
    , ast :: [TsNode]
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