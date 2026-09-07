-- | Hand-built 'AstModule's, for specs that need a shape rather than a parse.
module Fixtures.Deslop.AST (
    mkModule,
    mkModuleAt,
    mkModuleNamed,
    mkImportNode,
    mkReExportNode,
) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    EdgeKind (..),
    EdgeTarget (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import TestUtils (ap)

{- | Constructs an AstModule from its name and dependency targets, giving it a
source file directly under @\/home\/repo@ so that reported locations are
predictable.

Identity and name coincide here, which is what a spec wants unless it is about
the difference between them - see 'mkModuleNamed'.
-}
mkModule :: Text -> [Text] -> AstModule
mkModule name = mkModuleAt ("/home/repo/" <> name <> ".ts") name

-- | As 'mkModule', for a spec that cares where the source file sits.
mkModuleAt :: Text -> Text -> [Text] -> AstModule
mkModuleAt path name = mkModuleNamed path name (name :| [])

-- | As 'mkModule', but for a module that answers to more than one name.
mkModuleNamed :: Text -> Text -> NonEmpty Text -> [Text] -> AstModule
mkModuleNamed path moduleId names deps =
    AstModule
        { id = moduleIdUnsafe moduleId
        , names = moduleNameUnsafe <$> names
        , path = ap path
        , nodes = map mkImportNode deps
        }

-- | An import edge with a realistic raw statement.
mkImportNode :: Text -> AstNode
mkImportNode = mkEdge ImportEdge "import { ... } from '"

-- | A re-export edge with a realistic raw statement.
mkReExportNode :: Text -> AstNode
mkReExportNode = mkEdge ReExportEdge "export * from '"

mkEdge :: EdgeKind -> Text -> Text -> AstNode
mkEdge kind statementPrefix t =
    DependencyEdge
        { specifier = moduleNameUnsafe t
        , target = ToModule (moduleIdUnsafe t)
        , kind = kind
        , rawStatement = statementPrefix <> t <> "'"
        }
