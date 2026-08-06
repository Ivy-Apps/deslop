module Deslop.AST (
    AstNode (..),
    AstModule (..),
    parseAst,
) where

import Effectful
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (AbsPath (..), RoFileSystem, decodeOsPath)
import TypeScript.CST (TsNode (..), TsProgram (cst, path))
import TypeScript.Config (TsConfig)
import TypeScript.ModuleResolver (ModuleId (..), dropTypeScriptExtension, moduleIdUnsafe, reverseResolveImport)

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

parseAst :: (Reader TsConfig :> es, RoFileSystem :> es) => TsProgram -> Eff es AstModule
parseAst prog = do
    moduleId <- programModuleId
    pure
        AstModule
            { id = moduleId
            , path = prog.path
            , nodes = mapMaybe parseNode prog.cst
            }
  where
    programModuleId =
        reverseResolveImport prog.path
            . moduleIdUnsafe
            . decodeOsPath
            . dropTypeScriptExtension
            $ prog.path.osPath
    parseNode :: TsNode -> Maybe AstNode
    parseNode (Import pre t suf) =
        Just $
            ImportNode
                { target = moduleIdUnsafe t
                , rawStatement = pre <> t <> suf
                }
    parseNode _ = Nothing
