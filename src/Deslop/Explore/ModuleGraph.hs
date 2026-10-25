{- | A project's module graph, tagged with everything a reader needs to make
sense of it, ready to be explored in a browser.

The graph itself is the project as the frontend saw it: every parsed module, the
folders they sit in, the packages they reach for, and one 'Edge' per pair of
modules and kind of dependency, counting how often it is written. A module the
frontend resolved but never parsed has no place in it.

What makes it readable is computed here, so the browser only has to draw:

* a 'layer' per module, from "Deslop.Explore.Layering": entry points on top,
  modules importing nothing of the project at the bottom;
* 'implied' on every edge transitive reduction removes, so the default view
  draws only the arrows that carry information;
* 'hub' on a module so widely imported that its arrows would drown the rest;
* the 'ModuleRole' its frontend gave a module, and the tests it named the
  module the subject of;
* 'changed' on a module whose file differs from the diff base;
* the 'SourceBase' a module's path is appended to, to read it on the web.

Pure structure, like the rest of the core: no Problem, cycle or Rule is drawn.
What the page reads is "Deslop.Explore.ModuleGraph.Json", and writing it out
as the explorer page is "Deslop.Explore.Html".
-}
module Deslop.Explore.ModuleGraph (
    Diagram (..),
    ModuleNode (..),
    FolderNode (..),
    Edge (..),
    NodeId (..),
    FolderPath (..),
    diagramOf,
    hubShare,
    hubMinImporters,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Deslop.Explore.Layering (impliedEdges, layers)
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    ModuleName (..),
    ModuleRole (..),
    canonicalName,
 )
import FileSystem.Path (ProjectRelativePath, portableSegments)
import Git.SourceLink (Diff (..), GitRef, PullRequest (..), SourceLinks (..))

{- | A module is a hub when more than this share of the project's other
source modules import it.
-}
hubShare :: Rational
hubShare = 2 / 5

{- | Nor is it a hub with fewer importers than this, however small the project:
in a five-module project, two importers are not "everywhere".
-}
hubMinImporters :: Int
hubMinImporters = 3

data Diagram = Diagram
    { modules :: [ModuleNode]
    , folders :: [FolderNode]
    , externals :: [ModuleName]
    -- ^ Every package a module depends on, by the name the frontend gave it.
    , edges :: [Edge]
    , sourceLinks :: Maybe SourceLinks
    -- ^ None when the project is not on a forge we can link to.
    , pullRequest :: Maybe PullRequest
    -- ^ Where the changed files' diff is read. None without one, or when it could not be found.
    , diffBase :: Maybe GitRef
    -- ^ The ref the changes are counted from. None when no diff could be taken.
    }
    deriving stock (Show, Eq)

data ModuleNode = ModuleNode
    { path :: ProjectRelativePath
    -- ^ Where the module is. Also what identifies it here.
    , name :: ModuleName
    , aliases :: [ModuleName]
    , folder :: Maybe FolderPath
    -- ^ None for a module directly in the project root.
    , layer :: Int
    {- ^ 0 at the top. A module's layer is always above, so smaller than,
    that of everything it depends on outside its own cycle.
    -}
    , hub :: Bool
    , role :: ModuleRole
    , testedBy :: [ProjectRelativePath]
    -- ^ The tests its frontend named it the subject of.
    , changed :: Bool
    -- ^ Its file differs from the diff base, committed or not.
    }
    deriving stock (Show, Eq)

data FolderNode = FolderNode
    { path :: FolderPath
    , parent :: Maybe FolderPath
    -- ^ None for a folder directly in the project root.
    }
    deriving stock (Show, Eq)

-- | A folder's segments, outermost first.
newtype FolderPath = FolderPath (NonEmpty Text)
    deriving stock (Show, Eq, Ord)

-- | Every dependency of one kind from one module to another, however often written.
data Edge = Edge
    { source :: NodeId
    , target :: NodeId
    , kind :: EdgeKind
    , count :: Int
    , implied :: Bool
    -- ^ Removed by transitive reduction: the source reaches the target another way.
    }
    deriving stock (Show, Eq, Ord)

-- | What an edge joins. Modules are told apart by path, packages by name.
data NodeId
    = ModuleAt ProjectRelativePath
    | Package ModuleName
    deriving stock (Show, Eq, Ord)

{- | The diagram of these modules, given where their files and their diff can
be read and what the branch changed, when a diff could be taken.
-}
diagramOf :: Maybe SourceLinks -> Maybe PullRequest -> Maybe Diff -> [Module] -> Diagram
diagramOf sourceLinks pullRequest diff parsed =
    Diagram
        { modules = moduleNode <$> located
        , folders = folderNodes . mapMaybe (\(_, segs) -> folderOf segs) $ located
        , externals = ordNub [n | (_, Package n, _) <- deps]
        , edges = edgeOf <$> Map.toList (Map.fromListWith (+) [(d, 1 :: Int) | d <- deps])
        , sourceLinks
        , pullRequest
        , diffBase = (.base) <$> diff
        }
  where
    located = sortOn snd $ mapMaybe (\m -> (m,) <$> portableSegments m.path) parsed
    pathOf = Map.fromList [(m.id, m.path) | (m, _) <- located]

    deps =
        [ (ModuleAt m.path, to, e.kind)
        | (m, _) <- located
        , e <- m.edges
        , to <- maybeToList $ endOf e.target
        , to /= ModuleAt m.path
        ]
    endOf (Resolved mid _) = ModuleAt <$> Map.lookup mid pathOf
    endOf (External n) = Just $ Package n
    -- A dependency no frontend could name is no vertex of the graph, so nothing to draw.
    endOf (Unresolved _) = Nothing

    graph = Map.fromListWith (<>) $ [(m.path, Set.empty) | (m, _) <- located] <> [(a, Set.singleton b) | (ModuleAt a, ModuleAt b, _) <- deps]
    layerOf = layers graph
    implied = impliedEdges graph
    hubs = hubsOf graph (Set.fromList [m.path | (m, _) <- located, not $ isSource m.role])
    -- In the order the modules are drawn in, so each module's tests are too.
    testsOf = Map.fromListWith (flip (<>)) [(subject, [m.path]) | (m, _) <- located, Test (Just subject) <- [m.role]]

    moduleNode (m, segs) =
        ModuleNode
            { path = m.path
            , name = canonicalName m
            , aliases = NE.tail m.names
            , folder = folderOf segs
            , -- Never the default: 'layers' ranks every vertex, and every module is one.
              layer = Map.findWithDefault 0 m.path layerOf
            , hub = m.path `Set.member` hubs
            , role = m.role
            , testedBy = Map.findWithDefault [] m.id testsOf
            , changed = any (Set.member m.path . (.files)) diff
            }

    edgeOf ((from, to, k), n) =
        Edge
            { source = from
            , target = to
            , kind = k
            , count = n
            , implied = case (from, to) of
                (ModuleAt a, ModuleAt b) -> (a, b) `Set.member` implied
                _ -> False
            }

isSource :: ModuleRole -> Bool
isSource = \case
    Source -> True
    Test _ -> False
    Benchmark -> False

folderOf :: NonEmpty Text -> Maybe FolderPath
folderOf = fmap FolderPath . nonEmpty . NE.init

-- | Every folder holding a module, and every folder above one.
folderNodes :: [FolderPath] -> [FolderNode]
folderNodes leaves =
    [ FolderNode {path = f, parent = folderOf segs}
    | f@(FolderPath segs) <- Set.toList . Set.fromList $ concatMap ancestry leaves
    ]
  where
    ancestry (FolderPath segs) = mapMaybe (fmap FolderPath . nonEmpty) . drop 1 . inits $ NE.toList segs

{- | The modules more than 'hubShare' of the other source modules import, and
at least 'hubMinImporters' of them. A test or benchmark is never a hub: hiding
them already hides it.
-}
hubsOf :: (Ord k) => Map k (Set k) -> Set k -> Set k
hubsOf graph nonSources = Map.keysSet . Map.filterWithKey isHub $ importers
  where
    sources = Map.keysSet graph `Set.difference` nonSources
    importers =
        Map.fromListWith (<>) [(b, Set.singleton a) | (a, bs) <- Map.toList graph, a `Set.member` sources, b <- Set.toList bs, b /= a]
    others = Set.size sources - 1
    isHub b as =
        b `Set.member` sources
            && Set.size as >= hubMinImporters
            && toRational (Set.size as) > hubShare * toRational others
