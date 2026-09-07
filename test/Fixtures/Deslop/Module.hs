-- | Hand-built 'Module's, for specs that need a shape rather than a parse.
module Fixtures.Deslop.Module (
    mkModule,
    mkModuleAt,
    mkImportEdge,
    edgeTo,
) where

import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (ImportEdge),
    EdgeTarget (Resolved),
    Location (..),
    Module (..),
    Specifier (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import TestUtils (rp)

{- | Constructs a Module from its name and dependency targets, giving it a
source file directly under the project root so that reported locations are
predictable. Its dependencies are written one per line, in order.

Identity and name coincide here, and every dependency answers to exactly one
name, which is what a spec wants unless it is about the difference between them.
-}
mkModule :: Text -> [Text] -> Module
mkModule name = mkModuleAt (name <> ".ts") name

-- | As 'mkModule', for a spec that cares where the source file sits.
mkModuleAt :: Text -> Text -> [Text] -> Module
mkModuleAt path name deps =
    Module
        { id = moduleIdUnsafe name
        , names = moduleNameUnsafe name :| []
        , path = rp path
        , edges = zipWith (mkImportEdge path) [1 ..] deps
        }

{- | An import edge resolving to a module of the same name, written at a given
line of a given file.
-}
mkImportEdge :: Text -> Int -> Text -> DependencyEdge
mkImportEdge file line dep =
    DependencyEdge
        { specifier = specifierUnsafe dep
        , target = Resolved (moduleIdUnsafe dep) (moduleNameUnsafe dep :| [])
        , kind = ImportEdge
        , location =
            Location
                { file = rp file
                , line = line
                , code = "import { ... } from '" <> dep <> "'"
                }
        }

{- | The 'Location' 'mkModule' gave the edge naming @dep@.

Lets a spec about rule enforcement say where a violation points without
restating how the fixture lays a module out. Where the line itself is the
behaviour under test, "TypeScript.ModuleSpec" and P13-P15 assert it directly.
-}
edgeTo :: Module -> Text -> Location
edgeTo m dep =
    maybe (error $ "no edge to " <> dep) (.location)
        . find ((== dep) . (.specifier.text))
        $ m.edges
