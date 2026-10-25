{- | The 'Diagram' as the explorer page reads it: the @deslop-graph/1@ wire
schema.

Versioned by @schema@ so a page can refuse data it does not understand. Every
node is one of three kinds, told apart by @kind@ and by an id that never
collides across them; what the page filters and styles by is a node's or an
edge's @tags@.
-}
module Deslop.Explore.ModuleGraph.Json (graphJson) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.Explore.ModuleGraph (
    Diagram (..),
    Edge (..),
    FolderNode (..),
    FolderPath (..),
    ModuleNode (..),
    NodeId (..),
    hubMinImporters,
    hubShare,
 )
import Deslop.Module (EdgeKind (..), ModuleName (..), ModuleRole (..))
import FileSystem.Path (portablePath)
import Git.SourceLink (DiffAnchors (..), Forge (..), PullRequest (..), SourceBase (..), SourceLinks (..))

graphJson :: Diagram -> Value
graphJson d =
    object
        [ "schema" .= ("deslop-graph/1" :: Text)
        , "nodes" .= (fmap folderJson d.folders <> fmap moduleJson d.modules <> fmap externalJson d.externals)
        , "edges" .= fmap edgeJson d.edges
        , "sourceBase" .= fmap (.source.url) d.sourceLinks
        , "pullRequest" .= fmap (.url) d.pullRequest
        , "repoPrefix" .= (diffAnchors . (.forge) =<< d.sourceLinks)
        , "hub" .= object ["share" .= (fromRational hubShare :: Double), "minimum" .= hubMinImporters]
        ]

-- | The project's folder in the repository, on the one forge whose diffs are linked to.
diffAnchors :: Forge -> Maybe Text
diffAnchors = \case
    GitHub anchors -> Just anchors.repoPrefix
    GitLab -> Nothing

{- | What the page filters and styles a node or an edge by. A node or edge
carries each that holds of it, in this order.
-}
data Tag
    = HubTag
    | TestTag
    | BenchTag
    | ChangedTag
    | ImpliedTag
    | ExternalTag
    deriving stock (Show, Eq)

instance ToJSON Tag where
    toJSON =
        toJSON @Text . \case
            HubTag -> "hub"
            TestTag -> "test"
            BenchTag -> "bench"
            ChangedTag -> "changed"
            ImpliedTag -> "implied"
            ExternalTag -> "external"

moduleJson :: ModuleNode -> Value
moduleJson m =
    object
        [ "id" .= nodeId (ModuleAt m.path)
        , "kind" .= ("module" :: Text)
        , "name" .= m.name.text
        , "aliases" .= fmap (.text) m.aliases
        , "path" .= portablePath m.path
        , "parent" .= fmap folderId m.folder
        , "layer" .= m.layer
        , "tags" .= ([HubTag | m.hub] <> maybeToList roleTag <> [ChangedTag | m.changed])
        , "testedBy" .= fmap portablePath m.testedBy
        ]
  where
    roleTag = case m.role of
        Source -> Nothing
        Test _ -> Just TestTag
        Benchmark -> Just BenchTag

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
        , "tags" .= [ExternalTag]
        ]

edgeJson :: Edge -> Value
edgeJson e =
    object
        [ "source" .= nodeId e.source
        , "target" .= nodeId e.target
        , "kind" .= kindText e.kind
        , "count" .= e.count
        , "tags" .= ([ImpliedTag | e.implied] <> [ExternalTag | isPackage e.target])
        ]
  where
    isPackage (Package _) = True
    isPackage (ModuleAt _) = False

kindText :: EdgeKind -> Text
kindText ImportEdge = "import"
kindText ReExportEdge = "reexport"

{- | Ids never collide across kinds: a module's is its path, a folder's ends in
@/@, and a package's starts with @external:@, which no relative path does.
-}
nodeId :: NodeId -> Text
nodeId (ModuleAt p) = portablePath p
nodeId (Package n) = "external:" <> n.text

folderId :: FolderPath -> Text
folderId = (<> "/") . folderText

folderText :: FolderPath -> Text
folderText (FolderPath segs) = T.intercalate "/" $ NE.toList segs

folderLabel :: FolderPath -> Text
folderLabel (FolderPath segs) = NE.last segs
