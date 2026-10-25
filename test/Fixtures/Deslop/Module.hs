-- | Hand-built 'Module's, for specs that need a shape rather than a parse.
module Fixtures.Deslop.Module (
    mkModule,
    mkModuleAt,
    mkPathModule,
    mkTestAt,
    mkTestOf,
    mkBenchAt,
    mkImportEdge,
    mkEdge,
    resolvedTo,
    withUnresolved,
    edgeTo,
) where

import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (ImportEdge),
    EdgeTarget (Resolved, Unresolved),
    Location (..),
    Module (..),
    ModuleRole (..),
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
        , role = Source
        }

-- | A module at a path, named and identified by it, importing modules by their paths.
mkPathModule :: Text -> [Text] -> Module
mkPathModule path = mkModuleAt path path

-- | As 'mkPathModule', for a test its frontend named no subject of.
mkTestAt :: Text -> [Text] -> Module
mkTestAt path deps = (mkPathModule path deps) {role = Test Nothing}

-- | A test its frontend named the module at this path the subject of.
mkTestOf :: Text -> Text -> Module
mkTestOf path subject = (mkPathModule path []) {role = Test (Just $ moduleIdUnsafe subject)}

-- | As 'mkPathModule', for a benchmark.
mkBenchAt :: Text -> [Text] -> Module
mkBenchAt path deps = (mkPathModule path deps) {role = Benchmark}

{- | An import edge resolving to a module of the same name, written at a given
line of a given file.
-}
mkImportEdge :: Text -> Int -> Text -> DependencyEdge
mkImportEdge file line dep = mkEdge file line dep (resolvedTo dep) ImportEdge

-- | A dependency resolved to the module of this name, which answers to no other.
resolvedTo :: Text -> EdgeTarget
resolvedTo name = Resolved (moduleIdUnsafe name) (moduleNameUnsafe name :| [])

{- | As 'mkImportEdge', for a spec that cares what the edge resolved to and
whether it imports or re-exports.
-}
mkEdge :: Text -> Int -> Text -> EdgeTarget -> EdgeKind -> DependencyEdge
mkEdge file line dep target kind =
    DependencyEdge
        { specifier = specifierUnsafe dep
        , target = target
        , kind = kind
        , location =
            Location
                { file = rp file
                , line = line
                , code = "import { ... } from '" <> dep <> "'"
                }
        }

{- | The module with one more dependency, written on the line after its last,
that its frontend knows exists and cannot name. @spelling@ is what the author
wrote, so 'edgeTo' finds it by that.
-}
withUnresolved :: Text -> Module -> Module
withUnresolved spelling m = m {edges = m.edges <> [unresolved]}
  where
    unresolved =
        DependencyEdge
            { specifier = specifierUnsafe spelling
            , target = Unresolved (specifierUnsafe spelling)
            , kind = ImportEdge
            , location =
                Location
                    { file = m.path
                    , line = length m.edges + 1
                    , code = "import (" <> spelling <> ")"
                    }
            }

{- | The 'Location' 'mkModule' gave the edge naming @dep@.

Lets a spec about rule enforcement say where a violation points without
restating how the fixture lays a module out. Where the line itself is the
behaviour under test, "Frontend.TypeScript.ModuleSpec" and P13-P15 assert it directly.
-}
edgeTo :: Module -> Text -> Location
edgeTo m dep =
    maybe (error $ "no edge to " <> dep) (.location)
        . find ((== dep) . (.specifier.text))
        $ m.edges
