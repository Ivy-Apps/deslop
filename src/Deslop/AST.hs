module Deslop.AST (
    AstNode (..),
    AstModule (..),
    parseAst,
) where

import Deslop.RelativeImports (fixTarget)
import Effectful
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (RoFileSystem, decodeOsPath)
import TypeScript.CST (TsNode (..), TsProgram (cst, path))
import TypeScript.Config (TsConfig)
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)

newtype AstNode = ImportNode
    { target :: ModuleId
    }
    deriving stock (Show, Eq)
data AstModule = AstModule
    { id :: ModuleId
    , nodes :: [AstNode]
    }
    deriving stock (Show, Eq)

parseAst :: (Reader TsConfig :> es, RoFileSystem :> es) => TsProgram -> Eff es AstModule
parseAst prog = do
    moduleId <- programModuleId
    pure
        AstModule
            { id = moduleId
            , nodes = mapMaybe parseNode prog.cst
            }
  where
    programModuleId = fixTarget prog.path (decodeOsPath prog.path)
    parseNode :: TsNode -> Maybe AstNode
    parseNode (Import _ t _) =
        Just $
            ImportNode
                { target = moduleIdUnsafe t
                }
    parseNode _ = Nothing
