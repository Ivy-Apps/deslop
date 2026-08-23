-- | Hand-built 'AstModule's, for specs that need a shape rather than a parse.
module Fixtures.Deslop.AST (
    mkModule,
    mkImportNode,
) where

import Deslop.AST (AstModule (..), AstNode (..), moduleIdUnsafe)
import TestUtils (ap)

{- | Constructs an AstModule from its id and import targets, giving it a source
file under 'Fixtures.TypeScript.Config.defaultTsConfig's baseUrl so that
reported locations are predictable.
-}
mkModule :: Text -> [Text] -> AstModule
mkModule mid deps =
    AstModule
        { id = moduleIdUnsafe mid
        , path = ap ("/home/repo/" <> mid <> ".ts")
        , nodes = map mkImportNode deps
        }

-- | Constructs an ImportNode with a realistic raw import statement.
mkImportNode :: Text -> AstNode
mkImportNode t =
    ImportNode
        { target = moduleIdUnsafe t
        , rawStatement = "import { ... } from '" <> t <> "'"
        }
