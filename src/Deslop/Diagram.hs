{- | A picture of a project's structure: which folders depend on which, and
how heavily.

The tree is collapsed to 'overviewDepth' folder levels below the directory every
module shares, so the overview stays readable however deep the project nests.
Each folder of the overview is drilled into on its own 'Board', where its files
appear one by one and every other folder they touch stays one collapsed box.

Pure structure: no Problem, cycle or Rule is drawn. Only dependencies between
modules the frontend parsed are; an npm package or an unscanned module has no
place in the picture. Writing it out as D2 is 'toD2'.
-}
module Deslop.Diagram (
    Diagram (..),
    Graph (..),
    Arrow (..),
    Board (..),
    BoardEnd (..),
    BoardName (..),
    Place (..),
    Folder (..),
    Spot (..),
    Layout (..),
    overviewDepth,
    diagramOf,
    layoutOf,
    placeOf,
    toD2,
) where

import Data.Char (isAlphaNum, isAscii)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Diagram.D2 (Document (..), Key, Statement (..), Value (..), keyword, name)
import Deslop.Module (DependencyEdge (..), EdgeKind (..), EdgeTarget (..), Module (..), ModuleId)
import FileSystem.Path (portablePath)

-- | How many folder levels below the common prefix the overview keeps apart.
overviewDepth :: Int
overviewDepth = 2

data Diagram = Diagram
    { prefix :: [Text]
    -- ^ The directory every module sits under, stripped from every label.
    , overview :: Graph Place
    , boards :: [Board]
    -- ^ One per folder of the overview, in folder order.
    }
    deriving stock (Show, Eq)

-- | What one board draws: its boxes, and how many dependencies each arrow stands for.
data Graph n = Graph
    { nodes :: Set n
    , arrows :: Map (Arrow n) Int
    }
    deriving stock (Show, Eq)

-- | Dependencies of one kind from one box to another.
data Arrow n = Arrow
    { from :: n
    , to :: n
    , kind :: EdgeKind
    }
    deriving stock (Show, Eq, Ord, Functor)

-- | A folder of the overview, drilled into.
data Board = Board
    { folder :: Folder
    , name :: BoardName
    , graph :: Graph BoardEnd
    }
    deriving stock (Show, Eq)

-- | What a board draws a dependency between.
data BoardEnd
    = -- | A file of the board's own folder: its segments below that folder.
      Local (NonEmpty Text)
    | -- | Anything elsewhere, collapsed to its box on the overview.
      Neighbour Place
    deriving stock (Show, Eq, Ord)

{- | What D2 calls a board, and so what its SVG is called: ASCII, lowercase and
unique within the diagram, because a folder's own name may be none of these.
-}
newtype BoardName = BoardName Text
    deriving stock (Show, Eq, Ord)

-- | A box on the overview.
data Place
    = InFolder Folder
    | -- | A file directly in the common prefix, with no folder to collapse into.
      AtRoot Text
    deriving stock (Show, Eq, Ord)

{- | A folder at overview granularity: its segments below the common prefix,
never more than 'overviewDepth' of them.
-}
newtype Folder = Folder (NonEmpty Text)
    deriving stock (Show, Eq, Ord)

-- | Where one module sits once the tree is collapsed.
data Spot
    = -- | In a folder, and where within it: sub-folders, then the file.
      FolderFile Folder (NonEmpty Text)
    | RootFile Text
    deriving stock (Show, Eq, Ord)

data Layout = Layout
    { prefix :: [Text]
    , spots :: Map ModuleId Spot
    }
    deriving stock (Show, Eq)

diagramOf :: [Module] -> Diagram
diagramOf modules =
    Diagram
        { prefix = layout.prefix
        , overview =
            Graph
                { nodes = Set.fromList $ placeOf <$> spots
                , arrows = tally $ fmap placeOf <$> deps
                }
        , boards = boardOf spots deps <$> Map.toList (boardNames folders)
        }
  where
    layout = layoutOf modules
    spots = Map.elems layout.spots
    deps = dependencies layout modules
    folders = ordNub [f | FolderFile f _ <- spots]

{- | Where every module sits, below the directory they all share. A module
whose path names no file at all has nowhere to be drawn and is left out.
-}
layoutOf :: [Module] -> Layout
layoutOf modules =
    Layout
        { prefix = common
        , spots = Map.fromList [(m.id, spotOf . below $ segs) | (m, segs) <- located]
        }
  where
    located = mapMaybe (\m -> (m,) <$> pathSegments m) modules
    common = commonPrefix $ NE.init . snd <$> located
    -- The prefix is directories only, so the file itself always remains.
    below segs = NE.prependList (drop (length common) $ NE.init segs) (NE.last segs :| [])

pathSegments :: Module -> Maybe (NonEmpty Text)
pathSegments = nonEmpty . filter (`notElem` ["", "."]) . T.splitOn "/" . portablePath . (.path)

commonPrefix :: [[Text]] -> [Text]
commonPrefix [] = []
commonPrefix (x : xs) = foldl' shared x xs
  where
    shared a b = fmap fst . takeWhile (uncurry (==)) $ zip a b

-- | A module's path below the prefix, collapsed to 'overviewDepth' folders.
spotOf :: NonEmpty Text -> Spot
spotOf segs = case nonEmpty folder of
    Nothing -> RootFile file
    Just f -> FolderFile (Folder f) (NE.prependList inner (file :| []))
  where
    (folder, inner) = splitAt overviewDepth $ NE.init segs
    file = NE.last segs

placeOf :: Spot -> Place
placeOf (FolderFile f _) = InFolder f
placeOf (RootFile file) = AtRoot file

{- | Every dependency between two modules the frontend parsed, once per edge
written. Packages, and modules resolved but never parsed, are skipped: there is
nowhere to draw them.
-}
dependencies :: Layout -> [Module] -> [Arrow Spot]
dependencies layout modules =
    [ Arrow {from = from, to = to, kind = e.kind}
    | m <- modules
    , Just from <- [spotFor m.id]
    , e <- m.edges
    , Resolved target _ <- [e.target]
    , Just to <- [spotFor target]
    ]
  where
    spotFor mid = Map.lookup mid layout.spots

-- | How many dependencies each arrow stands for. A box depending on itself draws nothing.
tally :: (Ord n) => [Arrow n] -> Map (Arrow n) Int
tally = Map.fromListWith (+) . fmap (,1) . filter (\a -> a.from /= a.to)

boardOf :: [Spot] -> [Arrow Spot] -> (Folder, BoardName) -> Board
boardOf spots deps (f, boardName) =
    Board
        { folder = f
        , name = boardName
        , graph =
            Graph
                { nodes = Set.fromList $ locals <> concatMap (\a -> [a.from, a.to]) (Map.keys arrows)
                , arrows = arrows
                }
        }
  where
    locals = [Local inner | FolderFile g inner <- spots, g == f]
    arrows = tally . filter touchesBoard $ fmap (endIn f) <$> deps
    touchesBoard a = isLocal a.from || isLocal a.to

endIn :: Folder -> Spot -> BoardEnd
endIn f (FolderFile g inner) | g == f = Local inner
endIn _ spot = Neighbour $ placeOf spot

isLocal :: BoardEnd -> Bool
isLocal (Local _) = True
isLocal (Neighbour _) = False

-- | A name per folder, in folder order. @index@ is D2's own, for the overview.
boardNames :: [Folder] -> Map Folder BoardName
boardNames = fst . foldl' assign (Map.empty, Set.singleton "index") . sort
  where
    assign (names, taken) f =
        let n = fresh taken $ slug f
         in (Map.insert f (BoardName n) names, Set.insert n taken)
    fresh taken s =
        fromMaybe s . find (`Set.notMember` taken) $ s : [s <> "-" <> show i | i <- [2 :: Int ..]]

slug :: Folder -> Text
slug (Folder segs) = case T.intercalate "-" . filter (not . T.null) . T.split (not . safe) . T.toLower . T.intercalate "/" . NE.toList $ segs of
    "" -> "folder"
    s -> s
  where
    safe c = isAscii c && isAlphaNum c

--------------------------------------------------------------------------------
-- D2
--------------------------------------------------------------------------------

-- | The diagram as one D2 document: the overview, and every board as a layer of it.
toD2 :: Diagram -> Document
toD2 d =
    Document $
        [config, direction]
            <> graphStatements placeKey (placeShape FromRoot) d.overview
            <> [Block (kw "layers") (boardBlock <$> d.boards) | not (null d.boards)]
  where
    links = Map.fromList [(b.folder, b.name) | b <- d.boards]

    config =
        Block
            (kw "vars")
            [ Block
                (kw "d2-config")
                [ Field (kw "layout-engine") (Bare "elk")
                , Field (kw "theme-id") (Bare "0")
                , Field (kw "dark-theme-id") (Bare "200")
                ]
            ]

    placeShape vantage p = case p of
        InFolder f ->
            [ Field (kw "shape") (Bare "package")
            , Field (kw "label") (Str $ folderLabel f)
            , Field (kw "tooltip") (Str $ fullPath (folderSegments f))
            ]
                <> [Field (kw "link") (linkFrom vantage b) | Just b <- [Map.lookup f links]]
        AtRoot file -> [Field (kw "tooltip") (Str $ fullPath [file])]

    -- A layer does not inherit it, so every board states it again.
    direction = Field (kw "direction") (Bare "right")

    boardBlock b =
        Block (name $ boardText b.name :| []) $
            [ direction
            , Block
                (name $ "← overview" :| [])
                [Field (kw "near") (Bare "top-left"), Field (kw "link") (Bare "_")]
            , Block
                (name $ folderLabel b.folder :| [])
                [ Field (kw "label") (Str $ folderLabel b.folder)
                , Field (kw "tooltip") (Str . fullPath $ folderSegments b.folder)
                ]
            ]
                <> graphStatements (endKey b.folder) (endShape b.folder) b.graph

    endShape f = \case
        Local inner -> [Field (kw "tooltip") (Str . fullPath $ folderSegments f <> NE.toList inner)]
        Neighbour p -> placeShape FromBoard p

    fullPath segs = T.intercalate "/" $ d.prefix <> segs

-- | Every node, then every arrow, each in its set's order.
graphStatements :: (n -> Key) -> (n -> [Statement]) -> Graph n -> [Statement]
graphStatements keyOf shapeOf g =
    [Block (keyOf n) (shapeOf n) | n <- Set.toList g.nodes]
        <> [Connection (keyOf a.from) (keyOf a.to) (arrowStyle a.kind count) | (a, count) <- Map.toList g.arrows]

arrowStyle :: EdgeKind -> Int -> [Statement]
arrowStyle k count =
    [ Field (kw "label") (Str $ show count)
    , Field (keyword $ "style" :| ["stroke-width"]) (Bare . show $ strokeWidth count)
    ]
        <> case k of
            ImportEdge -> []
            ReExportEdge -> [Field (keyword $ "style" :| ["stroke-dash"]) (Bare "3")]

-- | 1 for a single dependency, one more per doubling, never past 6.
strokeWidth :: Int -> Int
strokeWidth count = min 6 . (+ 1) . length . takeWhile (<= count) $ iterate (* 2) 2

-- | Where a link is followed from: D2 resolves a board path relative to the board holding it.
data Vantage = FromRoot | FromBoard

linkFrom :: Vantage -> BoardName -> Value
linkFrom FromRoot b = Bare $ "layers." <> boardText b
linkFrom FromBoard b = Bare $ "_.layers." <> boardText b

boardText :: BoardName -> Text
boardText (BoardName t) = t

placeKey :: Place -> Key
placeKey (InFolder f) = name $ folderLabel f :| []
placeKey (AtRoot file) = name $ file :| []

-- | A board's own files sit inside a box for its folder, nested by sub-folder.
endKey :: Folder -> BoardEnd -> Key
endKey f (Local inner) = name $ NE.cons (folderLabel f) inner
endKey _ (Neighbour p) = placeKey p

folderLabel :: Folder -> Text
folderLabel = T.intercalate "/" . folderSegments

folderSegments :: Folder -> [Text]
folderSegments (Folder segs) = NE.toList segs

kw :: Text -> Key
kw = keyword . (:| [])
