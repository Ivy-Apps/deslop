-- | Hand-built 'AstModule's, for specs that need a shape rather than a parse.
module Fixtures.Deslop.AST (
    mkModule,
    mkModuleAt,
    mkImportNode,
) where

import Deslop.AST (AstModule (..), AstNode (..), moduleIdUnsafe)
import TestUtils (ap)

{- | Constructs an AstModule from its id and import targets, giving it a source
file directly under @\/home\/repo@ so that reported locations are predictable.
-}
mkModule :: Text -> [Text] -> AstModule
mkModule mid = mkModuleAt ("/home/repo/" <> mid <> ".ts") mid

-- | As 'mkModule', for a spec that cares where the source file sits.
mkModuleAt :: Text -> Text -> [Text] -> AstModule
mkModuleAt path mid deps =
    AstModule
        { id = moduleIdUnsafe mid
        , path = ap path
        , nodes = map mkImportNode deps
        }

-- | Constructs an ImportNode with a realistic raw import statement.
mkImportNode :: Text -> AstNode
mkImportNode t =
    ImportNode
        { target = moduleIdUnsafe t
        , rawStatement = "import { ... } from '" <> t <> "'"
        }
