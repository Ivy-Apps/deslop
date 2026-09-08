{-# LANGUAGE OverloadedRecordDot #-}

module Deslop.CodeGraph (
    GraphKey (..),
    graphKeyOf,
    ModuleNode (..),
    ModuleRef (..),
    refName,
    ModuleGraph (..),
    ModuleCycle (..),
    buildModuleGraph,
    hasPath,
    refOfKey,
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
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Tree (Tree, flatten)
import Deslop.Module (
    DependencyEdge (..),
    EdgeTarget (..),
    Module (..),
    ModuleId,
    ModuleName,
    Specifier (..),
    moduleNameUnsafe,
 )
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)

{- | What a vertex is keyed by. Identity for anything a specifier resolved to,
and the written specifier for anything it did not, which is the only thing we
know about @react@.
-}
data GraphKey
    = ModuleKey ModuleId
    | ExternalKey Specifier
    deriving stock (Show, Eq, Ord)

{- | The vertex a dependency leads to. Pure, because the frontend already did
the resolving - see "Deslop.Module".
-}
graphKeyOf :: DependencyEdge -> GraphKey
graphKeyOf edge = case edge.target of
    Resolved mid _ -> ModuleKey mid
    External spec -> ExternalKey spec

{- | A vertex in the architectural graph. Three situations, kept apart because
they are three different facts about a dependency and only the first can be
followed any further.
-}
data ModuleNode
    = -- | Lowered by a frontend, so its own dependencies are known.
      ParsedModule Module
    | -- | Resolved to something real that nobody read: gitignored, outside the
      -- scanned tree, or not a source file at all. Its names come from the
      -- edge that reached it, because the frontend minted them when it
      -- resolved the specifier. It has no outgoing edges, so a chain through
      -- it stops here - a blind spot, and one worth being able to name.
      UnscannedModule (NonEmpty ModuleName)
    | -- | @react@: nothing in the project answers to it. A chain ending here
      -- has genuinely ended.
      ExternalModule Specifier
    deriving stock (Show, Eq)

{- | A vertex as the rest of Deslop sees it: how to find it again, and every
name it answers to, canonical first.
-}
data ModuleRef = ModuleRef
    { key :: GraphKey
    , names :: NonEmpty ModuleName
    }
    deriving stock (Show, Eq)

{- | The core graph environment.
Bundles the unboxed integer array with its O(log N) mapping functions.
-}
data ModuleGraph = ModuleGraph
    { graph :: Graph
    , nodeFromV :: Vertex -> (ModuleNode, GraphKey, [GraphKey])
    , vertexFromKey :: GraphKey -> Maybe Vertex
    , parsedNames :: Set ModuleName
    -- ^ Every name of every module actually lowered. What @exists@ asks about,
    -- so that a name nothing on disk answers to cannot satisfy it.
    }

{- | A circular import chain, listed in walk order and starting at the cycle's
canonical start. Every module appears exactly once - the closing edge from the
last module back to the start is implicit.
-}
newtype ModuleCycle = ModuleCycle
    { modules :: NonEmpty Module
    }
    deriving stock (Show, Eq)

{- | Constructs the ModuleGraph from the modules a frontend lowered.

Everything those modules reach also gets a vertex, or 'graphFromEdges' would
drop the edge silently and a chain would end without saying so. Nothing here
invents a name for one: an edge to something unparsed carries the names the
frontend minted when it resolved the specifier, and an edge to @react@ carries
only the specifier, which is all anyone knows.

Which is why 'Map.fromList' may keep whichever entry it likes when two edges
reach one vertex. They agree: both carry what the frontend made of the same
resolved target. Deslop used to pick the smallest specifier that reached a
file, and that was a choice between disagreeing answers - a gitignored file
reached as both @..\/shared\/x@ and @\@\/shared\/x@ came out under the
first, which is not a name at all.
-}
buildModuleGraph :: [Module] -> ModuleGraph
buildModuleGraph modules =
    let
        parsedKeys = Set.fromList [ModuleKey m.id | m <- modules]
        reached =
            Map.fromList
                [ (graphKeyOf edge, reachedNode edge)
                | m <- modules
                , edge <- m.edges
                ]
        unparsed = Map.withoutKeys reached parsedKeys

        parsedEdges =
            [ (ParsedModule m, ModuleKey m.id, map graphKeyOf m.edges)
            | m <- modules
            ]
        unparsedEdges =
            [ (node, key, [])
            | (key, node) <- Map.toList unparsed
            ]
        (g, nodeV, keyV) = graphFromEdges (parsedEdges ++ unparsedEdges)
     in
        ModuleGraph
            { graph = g
            , nodeFromV = nodeV
            , vertexFromKey = keyV
            , parsedNames = Set.fromList [n | m <- modules, n <- toList m.names]
            }
  where
    reachedNode :: DependencyEdge -> ModuleNode
    reachedNode edge = case edge.target of
        Resolved _ names -> UnscannedModule names
        External spec -> ExternalModule spec

-- | What the graph knows about the vertex a key leads to, if it has one.
refOfKey :: (Reader ModuleGraph :> es) => GraphKey -> Eff es (Maybe ModuleRef)
refOfKey key = do
    mg <- ask @ModuleGraph
    pure $ refOf mg <$> mg.vertexFromKey key

-- | Whether a module Deslop actually parsed answers to this name.
moduleExists :: (Reader ModuleGraph :> es) => ModuleName -> Eff es Bool
moduleExists name = do
    mg <- ask @ModuleGraph
    pure $ Set.member name mg.parsedNames

hasPath :: (Reader ModuleGraph :> es) => GraphKey -> GraphKey -> Eff es Bool
hasPath from to = do
    mg <- ask @ModuleGraph
    pure $ case (mg.vertexFromKey from, mg.vertexFromKey to) of
        (Just vFrom, Just vTo) -> path mg.graph vFrom vTo
        _ -> False

reachableFrom :: (Reader ModuleGraph :> es) => GraphKey -> Eff es [ModuleRef]
reachableFrom from = do
    mg <- ask @ModuleGraph
    pure $ case mg.vertexFromKey from of
        Nothing -> []
        Just vFrom -> refOf mg <$> reachable mg.graph vFrom

{- | Returns the shortest dependency path.
ASSUMES: A path is guaranteed to exist between @from@ and @to@.
-}
findKnownPath :: (Reader ModuleGraph :> es) => GraphKey -> GraphKey -> Eff es (NonEmpty ModuleRef)
findKnownPath from to = do
    mg <- ask @ModuleGraph
    pure $ case (mg.vertexFromKey from, mg.vertexFromKey to) of
        (Just vFrom, Just vTo) ->
            let
                -- Standard BFS loop using IntSet for O(1) cycle detection
                bfs :: IntSet -> Seq (Key, [Key]) -> NonEmpty ModuleRef
                bfs _ Seq.Empty =
                    error "Invariant violated: Path guaranteed but not found."
                bfs visited ((v, pathAcc) :<| queue)
                    | v == vTo =
                        -- Path found: Map to refs and reverse the accumulator
                        NE.fromList . map (refOf mg) . reverse $ (v : pathAcc)
                    | otherwise =
                        -- graph ! v is O(1) adjacency list lookup
                        let neighbors = filter (`IntSet.notMember` visited) (mg.graph ! v)

                            -- Mark neighbors as visited BEFORE enqueuing to prevent queue bloat
                            visited' = foldr IntSet.insert visited neighbors
                            queue' = foldl' (\q n -> q |> (n, v : pathAcc)) queue neighbors
                         in bfs visited' queue'
             in
                bfs (IntSet.singleton vFrom) (Seq.singleton (vFrom, []))
        _ -> error "Invariant violated: modules do not exist in graph."

{- | Finds every circular import chain in the graph, one per strongly connected
component. A component with more than one module is always cyclic; a lone module
is cyclic only when it imports itself.
-}
findCycles :: (Reader ModuleGraph :> es) => Eff es [ModuleCycle]
findCycles = do
    mg <- ask @ModuleGraph
    pure . mapMaybe (cycleOf mg) . scc $ mg.graph

{- | Reduces a strongly connected component to the shortest cycle through its
canonical start. Only a parsed module can occur here - everything else is built
without outgoing edges - so a component holding one is not a cycle.
-}
cycleOf :: ModuleGraph -> Tree Vertex -> Maybe ModuleCycle
cycleOf mg component = do
    vertices <- nonEmpty . flatten $ component
    loop <- shortestLoop mg (IntSet.fromList . toList $ vertices) (canonicalStart vertices)
    ModuleCycle <$> traverse (moduleOf mg) loop
  where
    canonicalStart :: NonEmpty Vertex -> Vertex
    canonicalStart = NE.head . NE.sortWith (nameOf mg)

{- | Breadth-first search for the shortest walk leading from @start@ back to
itself within @component@. Neighbours are visited in name order so that ties
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
            sortOn (nameOf mg)
                . filter (`IntSet.member` component)
                $ mg.graph ! v
        unseen = filter (`IntSet.notMember` visited) neighbors
        visited' = foldr IntSet.insert visited unseen
        queue' = foldl' (\q n -> q |> (n, n NE.<| walk)) queue unseen

-- | The one name a report gives what a ref points at.
refName :: ModuleRef -> ModuleName
refName = NE.head . (.names)

refOf :: ModuleGraph -> Vertex -> ModuleRef
refOf mg v =
    let (node, key, _) = mg.nodeFromV v
     in ModuleRef {key = key, names = namesOf node}

{- | The names a vertex answers to. This is the one place a 'Specifier' becomes
a 'ModuleName', and it is legitimate here and nowhere else: a Rule saying
@forbids: react@ has to have something to match against, and an external
specifier is never relative, so it does name the same thing to every reader.
-}
namesOf :: ModuleNode -> NonEmpty ModuleName
namesOf (ParsedModule m) = m.names
namesOf (UnscannedModule names) = names
namesOf (ExternalModule spec) = moduleNameUnsafe spec.text :| []

moduleOf :: ModuleGraph -> Vertex -> Maybe Module
moduleOf mg v = case mg.nodeFromV v of
    (ParsedModule m, _, _) -> Just m
    (UnscannedModule _, _, _) -> Nothing
    (ExternalModule _, _, _) -> Nothing

-- | The canonical name of a vertex, which is what orderings here are taken on.
nameOf :: ModuleGraph -> Vertex -> ModuleName
nameOf mg = refName . refOf mg
