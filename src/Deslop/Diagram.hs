{- | A project's module graph, tagged with everything a reader needs to make
sense of it, ready to be explored in a browser.

The graph itself is the project as the frontend saw it: every parsed module, the
folders they sit in, the packages they reach for, and one 'Edge' per pair of
modules and kind of dependency, counting how often it is written. A module the
frontend resolved but never parsed has no place in it.

What makes it readable is computed here, so the browser only has to draw:

* a 'layer' per module, from "Deslop.Diagram.Layering": entry points on top,
  modules importing nothing of the project at the bottom;
* 'implied' on every edge transitive reduction removes, so the default view
  draws only the arrows that carry information;
* 'hub' on a module so widely imported that its arrows would drown the rest;
* the 'Role' of a module, and which specs test it.

Pure structure, like the rest of the core: no Problem, cycle or Rule is drawn.
Writing it out as the explorer page is "Deslop.Diagram.Html".
-}
module Deslop.Diagram (
    Diagram (..),
    ModuleNode (..),
    FolderNode (..),
    Edge (..),
    NodeId (..),
    FolderPath (..),
    Role (..),
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
import Deslop.Diagram.Layering (impliedEdges, layers)
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    ModuleName (..),
    canonicalName,
 )
import FileSystem.Path (portablePath)

{- | A module is a hub when more than this share of the project's other
non-test modules import it.
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
    , role :: Role
    , testedBy :: [Text]
    -- ^ The paths of the specs named after it.
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

-- | Whether a module is part of the product, or exists to test it.
data Role = Source | Test
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

diagramOf :: [Module] -> Diagram
diagramOf parsed =
    Diagram
        { modules = moduleNode <$> located
        , folders = folderNodes . mapMaybe (\(_, segs) -> folderOf segs) $ located
        , externals = ordNub [n | (_, Package n, _) <- deps]
        , edges = edgeOf <$> Map.toList (Map.fromListWith (+) [(d, 1 :: Int) | d <- deps])
        }
  where
    located = sortOn snd $ mapMaybe (\m -> (m,) <$> segmentsOf m) parsed
    pathOf = Map.fromList [(m.id, T.intercalate "/" $ NE.toList segs) | (m, segs) <- located]
    roleOf = Map.fromList [(m.id, roleAt segs) | (m, segs) <- located]

    deps =
        [ (ModuleAt $ pathOf Map.! m.id, to, e.kind)
        | (m, _) <- located
        , e <- m.edges
        , to <- maybeToList $ endOf e.target
        , to /= ModuleAt (pathOf Map.! m.id)
        ]
    endOf (Resolved mid _) = ModuleAt <$> Map.lookup mid pathOf
    endOf (External n) = Just $ Package n

    graph = Map.fromListWith (<>) $ [(p, Set.empty) | p <- Map.elems pathOf] <> [(a, Set.singleton b) | (ModuleAt a, ModuleAt b, _) <- deps]
    layerOf = layers graph
    implied = impliedEdges graph
    hubs = hubsOf graph (Set.fromList [pathOf Map.! mid | (mid, Test) <- Map.toList roleOf])
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
at least 'hubMinImporters' of them. A test is never a hub: hiding tests already
hides it.
-}
hubsOf :: (Ord k) => Map k (Set k) -> Set k -> Set k
hubsOf graph tests = Map.keysSet . Map.filterWithKey isHub $ importers
  where
    sources = Map.keysSet graph `Set.difference` tests
    importers =
        Map.fromListWith (<>) [(b, Set.singleton a) | (a, bs) <- Map.toList graph, a `Set.member` sources, b <- Set.toList bs, b /= a]
    others = Set.size sources - 1
    isHub b as =
        b `Set.member` sources
            && Set.size as >= hubMinImporters
            && toRational (Set.size as) > hubShare * toRational others

{- | Whether a path is a test's, judged by where it sits and what it is called,
the same way in every language:

* under a top-level @test@, @tests@, @spec@ or @specs@ folder, or any
  @__tests__@ folder (Haskell, Jest);
* or named @*Spec@, @*.test.*@ or @*.spec.*@ (hspec, Jest, Vitest).
-}
roleAt :: NonEmpty Text -> Role
roleAt segs
    | NE.head segs `elem` ["test", "tests", "spec", "specs"] && length segs > 1 = Test
    | "__tests__" `elem` NE.init segs = Test
    | isJust . testSuffix . stem $ NE.last segs = Test
    | otherwise = Source

{- | Which specs test each source module: a spec tests the module whose path,
without its extension, ends in the spec's own path once its test folder and
its @Spec@ (or @PropSpec@, @.test@, @.spec@) suffix are dropped. So
@test\/Deslop\/DiagramSpec.hs@ tests @src\/Deslop\/Diagram.hs@, and
@src\/a\/b.test.ts@ tests @src\/a\/b.ts@. When several modules match, the one
nearest the root does.
-}
specsOf :: [(Text, NonEmpty Text, Role)] -> Map Text [Text]
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
            ]

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
        , "tags" .= tags [("hub", m.hub), ("test", m.role == Test)]
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
