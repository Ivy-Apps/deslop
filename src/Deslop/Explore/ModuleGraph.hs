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
* the 'ModuleRole' its frontend gave a module, and which specs test it;
* 'changed' on a module whose file differs from the diff base;
* the 'SourceBase' a module's path is appended to, to read it on the web.

Pure structure, like the rest of the core: no Problem, cycle or Rule is drawn.
Writing it out as the explorer page is "Deslop.Explore.Html".
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

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.List (isSuffixOf)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
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
import FileSystem.Path (ProjectRelativePath, portablePath)
import Git.SourceLink (DiffAnchors (..), Forge (..), PullRequest (..), SourceBase (..), SourceLinks (..))

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
    }
    deriving stock (Show, Eq)

data ModuleNode = ModuleNode
    { path :: Text
    -- ^ Where the module is, spelled with @/@. Also what identifies it here.
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
    , testedBy :: [Text]
    -- ^ The paths of the specs named after it.
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
    = ModuleAt Text
    | Package ModuleName
    deriving stock (Show, Eq, Ord)

{- | The diagram of these modules, given where their files and their diff can
be read and the files changed since the diff base.
-}
diagramOf :: Maybe SourceLinks -> Maybe PullRequest -> Set ProjectRelativePath -> [Module] -> Diagram
diagramOf sourceLinks pullRequest changedFiles parsed =
    Diagram
        { modules = moduleNode <$> located
        , folders = folderNodes . mapMaybe (\(_, segs) -> folderOf segs) $ located
        , externals = ordNub [n | (_, Package n, _) <- deps]
        , edges = edgeOf <$> Map.toList (Map.fromListWith (+) [(d, 1 :: Int) | d <- deps])
        , sourceLinks
        , pullRequest
        }
  where
    located = sortOn snd $ mapMaybe (\m -> (m,) <$> segmentsOf m) parsed
    pathOf = Map.fromList [(m.id, T.intercalate "/" $ NE.toList segs) | (m, segs) <- located]
    roleOf = Map.fromList [(m.id, m.role) | (m, _) <- located]
    changedPaths = Set.map portablePath changedFiles

    deps =
        [ (ModuleAt $ pathOf Map.! m.id, to, e.kind)
        | (m, _) <- located
        , e <- m.edges
        , to <- maybeToList $ endOf e.target
        , to /= ModuleAt (pathOf Map.! m.id)
        ]
    endOf (Resolved mid _) = ModuleAt <$> Map.lookup mid pathOf
    endOf (External n) = Just $ Package n
    -- A dependency no frontend could name is no vertex of the graph, so nothing to draw.
    endOf (Unresolved _) = Nothing

    graph = Map.fromListWith (<>) $ [(p, Set.empty) | p <- Map.elems pathOf] <> [(a, Set.singleton b) | (ModuleAt a, ModuleAt b, _) <- deps]
    layerOf = layers graph
    implied = impliedEdges graph
    hubs = hubsOf graph (Set.fromList [pathOf Map.! mid | (mid, role) <- Map.toList roleOf, role /= Source])
    specs = specsOf [(pathOf Map.! m.id, segs, roleOf Map.! m.id) | (m, segs) <- located]

    moduleNode (m, segs) =
        let p = pathOf Map.! m.id
         in ModuleNode
                { path = p
                , name = canonicalName m
                , aliases = NE.tail m.names
                , folder = folderOf segs
                , layer = layerOf Map.! p
                , hub = p `Set.member` hubs
                , role = roleOf Map.! m.id
                , testedBy = Map.findWithDefault [] p specs
                , changed = p `Set.member` changedPaths
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

{- | A module's path, split on @/@. One whose path names no file at all has
nowhere to be drawn.
-}
segmentsOf :: Module -> Maybe (NonEmpty Text)
segmentsOf = nonEmpty . filter (`notElem` ["", "."]) . T.splitOn "/" . portablePath . (.path)

folderOf :: NonEmpty Text -> Maybe FolderPath
folderOf = fmap FolderPath . nonEmpty . NE.init

-- | Every folder holding a module, and every folder above one.
folderNodes :: [FolderPath] -> [FolderNode]
folderNodes leaves =
    [ FolderNode {path = f, parent = FolderPath <$> nonEmpty (NE.init segs)}
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

{- | Which specs test each source module: a spec tests the module whose path,
without its extension, ends in the spec's own path once its test folder and
its @Spec@ (or @PropSpec@, @.test@, @.spec@) suffix are dropped. So
@test\/Deslop\/DiagramSpec.hs@ tests @src\/Deslop\/Diagram.hs@, and
@src\/a\/b.test.ts@ tests @src\/a\/b.ts@. When several modules match, the one
nearest the root does.
-}
specsOf :: [(Text, NonEmpty Text, ModuleRole)] -> Map Text [Text]
specsOf entries = Map.fromListWith (flip (<>)) [(subject, [spec]) | (spec, subject) <- matches]
  where
    sources = [(p, stemmed segs) | (p, segs, Source) <- entries]
    matches =
        [ (spec, p)
        | (spec, segs, Test) <- entries
        , Just key <- [subjectKey segs]
        , let candidates = [(length s, p') | (p', s) <- sources, key `isSuffixOf` s]
        , Just (_, p) <- [viaNonEmpty head $ sort candidates]
        ]

    stemmed segs = NE.init segs <> [stem $ NE.last segs]

    -- A helper under test/ that is named after nothing tests nothing.
    subjectKey segs = (\file -> folders segs <> [file]) <$> testSuffix (stem $ NE.last segs)
    folders = filter (/= "__tests__") . dropTestRoot . NE.init

    dropTestRoot = \case
        (top : rest) | top `elem` ["test", "tests", "spec", "specs"] -> rest
        other -> other

-- | A file's name without its extension.
stem :: Text -> Text
stem file = case T.breakOnEnd "." file of
    (before, _) | T.length before > 1 -> T.dropEnd 1 before
    _ -> file

-- | The subject's name, when a file is named as a test of it.
testSuffix :: Text -> Maybe Text
testSuffix name = viaNonEmpty head $ mapMaybe (\s -> T.stripSuffix s name >>= nonBlank) ["PropSpec", "Spec", ".test", ".spec"]
  where
    nonBlank t = if T.null t then Nothing else Just t

--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

{- | The graph as the explorer page reads it, versioned by @schema@ so a page
can refuse data it does not understand.
-}
instance ToJSON Diagram where
    toJSON d =
        object
            [ "schema" .= ("deslop-graph/1" :: Text)
            , "nodes" .= (fmap folderJson d.folders <> fmap moduleJson d.modules <> fmap externalJson d.externals)
            , "edges" .= fmap edgeJson d.edges
            , "sourceBase" .= fmap (.source.url) d.sourceLinks
            , "pullRequest" .= fmap (.url) d.pullRequest
            , "repoPrefix" .= (diffAnchors . (.forge) =<< d.sourceLinks)
            ]

-- | The project's folder in the repository, on the one forge whose diffs are linked to.
diffAnchors :: Forge -> Maybe Text
diffAnchors = \case
    GitHub anchors -> Just anchors.repoPrefix
    GitLab -> Nothing

moduleJson :: ModuleNode -> Value
moduleJson m =
    object
        [ "id" .= m.path
        , "kind" .= ("module" :: Text)
        , "name" .= m.name.text
        , "aliases" .= fmap (.text) m.aliases
        , "path" .= m.path
        , "parent" .= fmap folderId m.folder
        , "layer" .= m.layer
        , "tags" .= tags [("hub", m.hub), ("test", m.role == Test), ("bench", m.role == Benchmark), ("changed", m.changed)]
        , "testedBy" .= m.testedBy
        ]

folderJson :: FolderNode -> Value
folderJson f =
    object
        [ "id" .= folderId f.path
        , "kind" .= ("folder" :: Text)
        , "label" .= folderLabel f.path
        , "path" .= folderText f.path
        , "parent" .= fmap folderId f.parent
        ]

externalJson :: ModuleName -> Value
externalJson n =
    object
        [ "id" .= nodeId (Package n)
        , "kind" .= ("external" :: Text)
        , "name" .= n.text
        , "tags" .= ["external" :: Text]
        ]

edgeJson :: Edge -> Value
edgeJson e =
    object
        [ "source" .= nodeId e.source
        , "target" .= nodeId e.target
        , "kind" .= kindText e.kind
        , "count" .= e.count
        , "tags" .= tags [("implied", e.implied), ("external", isPackage e.target)]
        ]
  where
    isPackage (Package _) = True
    isPackage (ModuleAt _) = False

kindText :: EdgeKind -> Text
kindText ImportEdge = "import"
kindText ReExportEdge = "reexport"

tags :: [(Text, Bool)] -> [Text]
tags = fmap fst . filter snd

{- | Ids never collide across kinds: a module's is its path, a folder's ends in
@/@, and a package's starts with @external:@, which no relative path does.
-}
nodeId :: NodeId -> Text
nodeId (ModuleAt p) = p
nodeId (Package n) = "external:" <> n.text

folderId :: FolderPath -> Text
folderId = (<> "/") . folderText

folderText :: FolderPath -> Text
folderText (FolderPath segs) = T.intercalate "/" $ NE.toList segs

folderLabel :: FolderPath -> Text
folderLabel (FolderPath segs) = NE.last segs
