{-# LANGUAGE OverloadedRecordDot #-}

module Deslop.CodeGraph (
    ModuleNode (..),
    ModuleGraph (..),
    ModuleCycle (..),
    buildModuleGraph,
    hasPath,
    moduleExists,
    reachableFrom,
    findKnownPath,
    findCycles,
) where

import Data.Array ((!))
import Data.Graph (Graph, Vertex, graphFromEdges, path, reachable, scc)
import Data.IntSet (Key)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Tree (Tree, flatten)
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

{- | A circular import chain, listed in walk order and starting at the cycle's
canonical start. Every module appears exactly once - the closing edge from the
last module back to the start is implicit.
-}
newtype ModuleCycle = ModuleCycle
    { modules :: NonEmpty AstModule
    }
    deriving stock (Show, Eq)

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

moduleExists :: (Reader ModuleGraph :> es) => ModuleId -> Eff es Bool
moduleExists mid = do
    mg <- ask @ModuleGraph
    pure $ isJust (mg.vertexFromId mid)

hasPath :: (Reader ModuleGraph :> es) => ModuleId -> ModuleId -> Eff es Bool
hasPath from to = do
    mg <- ask @ModuleGraph
    pure $ case (mg.vertexFromId from, mg.vertexFromId to) of
        (Just vFrom, Just vTo) -> path mg.graph vFrom vTo
        _ -> False

reachableFrom :: (Reader ModuleGraph :> es) => ModuleId -> Eff es [ModuleId]
reachableFrom from = do
    mg <- ask @ModuleGraph
    pure $ case mg.vertexFromId from of
        Nothing -> []
        Just vFrom ->
            [ mid
            | v <- reachable mg.graph vFrom
            , let (_, mid, _) = mg.nodeFromV v
            ]

{- | Returns the shortest dependency path.
ASSUMES: A path is guaranteed to exist between @from@ and @to@.
-}
findKnownPath :: (Reader ModuleGraph :> es) => ModuleId -> ModuleId -> Eff es (NonEmpty ModuleId)
findKnownPath from to = do
    mg <- ask @ModuleGraph
    pure $ case (mg.vertexFromId from, mg.vertexFromId to) of
        (Just vFrom, Just vTo) ->
            let
                toId v = let (_, mid, _) = mg.nodeFromV v in mid

                -- Standard BFS loop using IntSet for O(1) cycle detection
                bfs :: IntSet -> Seq (Key, [Key]) -> NonEmpty ModuleId
                bfs _ Seq.Empty =
                    error "Invariant violated: Path guaranteed but not found."
                bfs visited ((v, pathAcc) :<| queue)
                    | v == vTo =
                        -- Path found: Map to IDs and reverse the accumulator
                        NE.fromList . map toId . reverse $ (v : pathAcc)
                    | otherwise =
                        -- graph ! v is O(1) adjacency list lookup
                        let neighbors = filter (`IntSet.notMember` visited) (mg.graph ! v)

                            -- Mark neighbors as visited BEFORE enqueuing to prevent queue bloat
                            visited' = foldr IntSet.insert visited neighbors
                            queue' = foldl' (\q n -> q |> (n, v : pathAcc)) queue neighbors
                         in bfs visited' queue'
             in
                bfs (IntSet.singleton vFrom) (Seq.singleton (vFrom, []))
        _ -> error "Invariant violated: ModuleIds do not exist in graph."

{- | Finds every circular import chain in the graph, one per strongly connected
component. A component with more than one module is always cyclic; a lone module
is cyclic only when it imports itself.
-}
findCycles :: (Reader ModuleGraph :> es) => Eff es [ModuleCycle]
findCycles = do
    mg <- ask @ModuleGraph
    pure . mapMaybe (cycleOf mg) . scc $ mg.graph

{- | Reduces a strongly connected component to the shortest cycle through its
canonical start. External modules cannot occur here - they are built without
outgoing edges - so a component holding one is not a cycle.
-}
cycleOf :: ModuleGraph -> Tree Vertex -> Maybe ModuleCycle
cycleOf mg component = do
    vertices <- nonEmpty . flatten $ component
    loop <- shortestLoop mg (IntSet.fromList . toList $ vertices) (canonicalStart vertices)
    ModuleCycle <$> traverse (astModuleOf mg) loop
  where
    canonicalStart :: NonEmpty Vertex -> Vertex
    canonicalStart = NE.head . NE.sortWith (moduleIdOf mg)

{- | Breadth-first search for the shortest walk leading from @start@ back to
itself within @component@. Neighbours are visited in ModuleId order so that ties
between equally short cycles resolve deterministically.
-}
shortestLoop :: ModuleGraph -> IntSet -> Vertex -> Maybe (NonEmpty Vertex)
shortestLoop mg component start = bfs (IntSet.singleton start) (Seq.singleton (start, start :| []))
  where
    bfs :: IntSet -> Seq (Vertex, NonEmpty Vertex) -> Maybe (NonEmpty Vertex)
    bfs _ Seq.Empty = Nothing
    bfs visited ((v, walk) :<| queue)
        | start `elem` neighbors = Just . NE.reverse $ walk
        | otherwise = bfs visited' queue'
      where
        neighbors =
            sortOn (moduleIdOf mg)
                . filter (`IntSet.member` component)
                $ mg.graph ! v
        unseen = filter (`IntSet.notMember` visited) neighbors
        visited' = foldr IntSet.insert visited unseen
        queue' = foldl' (\q n -> q |> (n, n NE.<| walk)) queue unseen

astModuleOf :: ModuleGraph -> Vertex -> Maybe AstModule
astModuleOf mg v = case mg.nodeFromV v of
    (InternalModule m, _, _) -> Just m
    (ExternalModule _, _, _) -> Nothing

moduleIdOf :: ModuleGraph -> Vertex -> ModuleId
moduleIdOf mg v = let (_, mid, _) = mg.nodeFromV v in mid
