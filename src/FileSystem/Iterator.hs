module FileSystem.Iterator (
    Entry (..),
    walkDir,
) where

import Effectful
import Effects.FileSystem (AbsPath, RoFileSystem, fsDirectoryExists, fsIsSymlink, fsListDirectory)

{- | A single directory listing entry, tagged with whether it is a directory.

The tag is not incidental: @.gitignore@ distinguishes @build/@ (directories
only) from @build@ (either), so any predicate over entries needs it.
-}
data Entry = Entry
    { path :: !AbsPath
    , isDir :: !Bool
    }
    deriving (Show, Eq)

{- | Recursively walks the tree rooted at the given directory.

@prune@ drops an entry entirely; for a directory that also means its whole
subtree is skipped. @select@ decides what each surviving entry contributes to
the result, and sees directories as well as files.

Symlinked directories are never descended into, matching git's own traversal.
That is what makes this terminate: a symlink loop produces ever-longer distinct
paths, so no visited-set over raw paths could detect it.
-}
walkDir ::
    (RoFileSystem :> es) =>
    (Entry -> Bool) ->
    (Entry -> Maybe a) ->
    AbsPath ->
    Eff es [a]
walkDir prune select = go
  where
    go dir = fsListDirectory dir >>= fmap concat . traverse visit

    visit p = do
        entry <- Entry p <$> fsDirectoryExists p
        bool (keep entry) (pure []) . prune $ entry

    keep entry = ((maybeToList . select $ entry) <>) <$> descend entry

    descend entry
        | entry.isDir = fsIsSymlink entry.path >>= bool (go entry.path) (pure [])
        | otherwise = pure []
