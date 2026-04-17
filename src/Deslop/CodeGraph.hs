{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Deslop.CodeGraph (
    GraphNode (..),
    CodebaseGraph (..),
) where

import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.Set qualified as Set
import Deslop.AST (AstModule (..), AstNode (..))
import TypeScript.ModuleResolver (ModuleId (..))

{- | Represents a node in the architectural graph.
It unifies parsed TypeScript files and unparsed 3rd-party dependencies
so both can exist as addressable vertices in the underlying integer array.
-}
data GraphNode
    = InternalModule AstModule
    | ExternalModule ModuleId
    deriving stock (Show, Eq)

{- | The core graph environment.
Bundles the unboxed integer array with its O(log N) mapping functions.
-}
data CodebaseGraph = CodebaseGraph
    { graph :: Graph
    , nodeFromV :: Vertex -> (GraphNode, ModuleId, [ModuleId])
    , vertexFromId :: ModuleId -> Maybe Vertex
    }

-- | Constructs the CodebaseGraph from a list of parsed AST modules.
buildCodebaseGraph :: [AstModule] -> CodebaseGraph
buildCodebaseGraph modules =
    let
        internalIds = Set.fromList [m.id | m <- modules]
        allTargets = Set.fromList [n.target | m <- modules, n <- m.nodes]
        externalIds = Set.difference allTargets internalIds

        internalEdges =
            [ (InternalModule m, m.id, map (.target) m.nodes)
            | m <- modules
            ]
        externalEdges =
            [ (ExternalModule extId, extId, [])
            | extId <- Set.toList externalIds
            ]
        (g, nodeV, keyV) = graphFromEdges (internalEdges ++ externalEdges)
     in
        CodebaseGraph
            { graph = g
            , nodeFromV = nodeV
            , vertexFromId = keyV
            }
