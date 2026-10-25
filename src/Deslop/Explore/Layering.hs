{- | What makes a large dependency graph drawable: which way is up, and which
arrows say nothing a reader could not already follow.

Both answers come from the graph's condensation, where every cycle collapses
into one 'Component' and what remains is acyclic:

* 'layers' ranks every vertex by the longest path below it, so a vertex always
  sits strictly above everything it reaches outside its own cycle.
* 'impliedEdges' is what transitive reduction removes: an edge @a -> b@ whose
  target is reachable from @a@ another way.

Polymorphic over the vertex, so it knows nothing of modules.
-}
module Deslop.Explore.Layering (
    Graph,
    Component,
    condense,
    layers,
    impliedEdges,
) where

import Data.Graph (flattenSCC, stronglyConnComp)
import Data.Map.Lazy qualified as LMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- | Every vertex is a key, mapped to the vertices it has an edge to.
type Graph k = Map k (Set k)

-- | A strongly connected component of a graph: one cycle, or a lone vertex.
newtype Component = Component Int
    deriving stock (Show, Eq, Ord)

{- | Each vertex's component, and the acyclic graph between components. An
edge inside one component is dropped; parallel edges merge.
-}
condense :: (Ord k) => Graph k -> (Map k Component, Graph Component)
condense g = (componentOf, Map.fromListWith (<>) $ ownKeys <> crossing)
  where
    sccs = flattenSCC <$> stronglyConnComp [(v, v, Set.toList ws) | (v, ws) <- Map.toList g]
    componentOf = Map.fromList [(v, Component i) | (i, vs) <- zip [0 ..] sccs, v <- vs]
    ownKeys = [(c, Set.empty) | c <- Map.elems componentOf]
    crossing =
        [ (cv, Set.singleton cw)
        | (v, ws) <- Map.toList g
        , let cv = componentOf Map.! v
        , w <- Set.toList ws
        , let cw = componentOf Map.! w
        , cv /= cw
        ]

{- | Every vertex's layer, 0 at the top. A vertex with no edge out of its own
component sits on the bottom layer, and every other one exactly one layer above
the highest thing it reaches: longest-path layering of the condensation, so the
members of a cycle share a layer.
-}
layers :: (Ord k) => Graph k -> Map k Int
layers g = (\c -> deepest - heights Map.! c) <$> componentOf
  where
    (componentOf, dag) = condense g
    -- Lazy, so each component's height is computed once, from those below it.
    heights = LMap.map height dag
    height = foldr (max . (+ 1) . (heights LMap.!)) 0
    deepest = foldr max 0 heights

{- | The edges between components that transitive reduction removes: those
whose target the source still reaches through some other edge. Every edge it
returns is one of the graph's own; none lies inside a cycle.
-}
impliedEdges :: (Ord k) => Graph k -> Set (k, k)
impliedEdges g =
    Set.fromList
        [ (v, w)
        | (v, ws) <- Map.toList g
        , let cv = componentOf Map.! v
        , w <- Set.toList ws
        , let cw = componentOf Map.! w
        , cv /= cw
        , redundant cv cw
        ]
  where
    (componentOf, dag) = condense g
    -- Lazy, so each component's reach is computed once, from those below it.
    reach = LMap.map (\cs -> cs <> foldMap (reach LMap.!) cs) dag
    redundant cv cw = any (\via -> via /= cw && cw `Set.member` (reach LMap.! via)) $ dag Map.! cv
