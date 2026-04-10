module Deslop.AST (
    ModuleId (..),
    AstNode (..),
    AstModule (..),
    parseAst,
) where

import Data.Text qualified as T
import Deslop.RelativeImports (fixTarget)
import Effectful
import Effectful.Reader.Static (Reader)
import FsEncoding (decodePathString)
import System.FilePath (dropExtension)
import TypeScript.CST (TsNode (..), TsProgram (cst, path))
import TypeScript.Config (TsConfigLegacy)

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

parseAst :: (Reader TsConfigLegacy :> es) => TsProgram -> Eff es AstModule
parseAst prog = do
    moduleId <- programModuleId
    pure
        AstModule
            { id = moduleId
            , nodes = mapMaybe parseNode prog.cst
            }
  where
    programModuleId =
        ModuleId . T.pack . dropExtension . T.unpack
            <$> fixTarget prog.path (T.pack $ decodePathString prog.path)
    parseNode :: TsNode -> Maybe AstNode
    parseNode (Import _ t _) =
        Just $
            ImportNode
                { target = ModuleId t
                }
    parseNode _ = Nothing
