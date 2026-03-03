module Deslop.AST (
    ModuleId (..),
    AstNode (..),
    AstModule (..),
    parseAst,
) where

import Data.Text (Text)
import TypeScript.CST (TsProgram)

newtype ModuleId = ModuleId Text deriving stock (Show, Eq)
newtype AstNode = ImportNode
    { target :: ModuleId
    }
    deriving stock (Show, Eq)
data AstModule = AstModule
    { id :: ModuleId
    , nodes :: [AstNode]
    }
    deriving stock (Show, Eq)

parseAst :: TsProgram -> AstModule
parseAst = undefined
