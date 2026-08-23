{- | Lowering a TypeScript concrete syntax tree into the language-agnostic
"Deslop.AST".

This is the seam a language crosses: everything above it reasons about
'AstModule's and knows nothing of TypeScript, and everything below it is
TypeScript all the way down to the bytes. A second language earns its support
by supplying a module of this shape and nothing more.
-}
module TypeScript.AST (parseAst) where

import Deslop.AST (AstModule (..), AstNode (..), moduleIdUnsafe)
import Effectful
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (RoFileSystem)
import FileSystem.Path (AbsPath (..), decodeOsPath)
import TypeScript.Config (TsConfig)
import TypeScript.CST (TsNode (..), TsProgram (cst, path))
import TypeScript.ModuleResolver (dropTypeScriptExtension, reverseResolveImport)

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
