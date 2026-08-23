{- | What Deslop reasons about, once a language frontend has done its work.

Deliberately small, and deliberately free of any one language: an 'AstModule'
is a module id, the file it came from, and the modules it reaches for. That is
everything "Deslop.CodeGraph" needs to build a graph and everything
"Deslop.Rule.Enforcer" needs to judge one, so it is everything a new language
has to produce to be supported.

Producing one from TypeScript is "TypeScript.AST".
-}
module Deslop.AST (
    ModuleId (text),
    moduleIdUnsafe,
    AstNode (..),
    AstModule (..),
) where

import FileSystem.Path (AbsPath)

{- | Logical module id - e.g. @/lib/util or /src/lib/util (relative to the nearest TS config)
or ./LoginView (relative to the current file) or ../../lib/util (relative to the current file)
or /home/repo/src/lib/util
-}
newtype ModuleId = ModuleId
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)

moduleIdUnsafe :: Text -> ModuleId
moduleIdUnsafe = ModuleId

data AstNode = ImportNode
    { target :: ModuleId
    , rawStatement :: Text
    }
    deriving stock (Show, Eq)

data AstModule = AstModule
    { id :: ModuleId
    , path :: AbsPath
    , nodes :: [AstNode]
    }
    deriving stock (Show, Eq)
