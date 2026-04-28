{-# LANGUAGE OverloadedRecordDot #-}

module Deslop.CodeGraph (
    ModuleNode (..),
    ModuleGraph (..),
    buildModuleGraph,
    hasPath,
) where

import Data.Graph (Graph, Vertex, graphFromEdges, path)
import Data.Set qualified as Set
import Deslop.AST (AstModule (..), AstNode (..))
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import TypeScript.ModuleResolver (ModuleId (..))

{- | Represents a node in the architectural graph.
It unifies parsed TypeScript files and unparsed 3rd-party dependencies
so both can exist as addressable vertices in the underlying integer array.
-}
data ModuleNode
    = InternalModule AstModule
    | ExternalModule ModuleId
    deriving stock (Show, Eq)

{- | The core graph environment.
Bundles the unboxed integer array with its O(log N) mapping functions.
-}
data ModuleGraph = ModuleGraph
    { graph :: Graph
    , nodeFromV :: Vertex -> (ModuleNode, ModuleId, [ModuleId])
    , vertexFromId :: ModuleId -> Maybe Vertex
    }

-- | Constructs the ModuleGraph from a list of parsed AST modules.
buildModuleGraph :: [AstModule] -> ModuleGraph
buildModuleGraph modules =
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
        ModuleGraph
            { graph = g
            , nodeFromV = nodeV
            , vertexFromId = keyV
            }

hasPath :: (Reader ModuleGraph :> es) => ModuleId -> ModuleId -> Eff es Bool
hasPath from to = do
    mg <- ask @ModuleGraph
    pure $ case (mg.vertexFromId from, mg.vertexFromId to) of
        (Just vFrom, Just vTo) -> path mg.graph vFrom vTo
        _ -> False
