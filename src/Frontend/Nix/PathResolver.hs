{-# LANGUAGE QuasiQuotes #-}

{- | What a path written in a Nix file reaches on disk.

A literal path reaches what is there: a file, or for a directory the
@default.nix@ in it - what @import@ would read - or, without one, the
directory itself, which is what a @src = .\/dir@ or a @builtins.readDir@
depends on. A path with a computed segment, @.\/plugins\/${name}.nix@,
reaches every entry its literal parts allow, one directory level per
segment: an over-approximation the pattern's base bounds. Nothing outside
the project is reached, not even through a symbolic link, and a path to
nothing reaches nothing.
-}
module Frontend.Nix.PathResolver (reachedBy) where

import Data.Text qualified as T
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsDirectoryExists, fsFileExists, fsListDirectory, fsMkAbsolute)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), absPathUnsafe, decodeOsPath, encodeOsPath)
import Frontend.Nix.Reference (PathPattern, Piece (..), Segment, literalOf)
import System.OsPath (OsPath, joinPath, osp, splitDirectories, takeFileName, (</>))

{- | Every file or directory the pattern reaches from the directory, inside
the project, in path order. Empty when it reaches nothing there.
-}
reachedBy :: (RoFileSystem :> es) => ProjectRoot -> AbsPath -> PathPattern -> Eff es [AbsPath]
reachedBy projectRoot base pattern = do
    candidates <- foldlM step [splitDirectories base.osPath] pattern
    settled <- catMaybes <$> traverse settle (filter (inside projectRoot.path) candidates)
    canonicalRoot <- fsMkAbsolute projectRoot.path.osPath
    sortNub <$> filterM (staysIn canonicalRoot) settled
  where
    -- Lexically first, so a walk never lists what is outside; then once
    -- every link is followed, as the 'Deslop.Module.ModuleId' will be.
    inside root segments = splitDirectories root.osPath `isPrefixOf` segments
    staysIn root path = inside root . splitDirectories . (.osPath) <$> fsMkAbsolute path.osPath

    step candidates segment = case literalOf segment of
        Just "." -> pure candidates
        Just ".." -> pure (mapMaybe parent candidates)
        Just name -> pure (fmap (<> [encodeOsPath name]) candidates)
        Nothing -> concat <$> traverse (children segment) candidates

    parent segments = case segments of
        [_] -> Nothing
        _ -> viaNonEmpty init segments

    children segment candidate = do
        let dir = absPathUnsafe (joinPath candidate)
        isDir <- fsDirectoryExists dir
        entries <- if isDir then fsListDirectory dir else pure []
        pure [candidate <> [name] | entry <- entries, let name = takeFileName entry.osPath, matchesSegment segment (decodeOsPath name)]

    settle segments = do
        let path = absPathUnsafe (joinPath segments)
        isFile <- fsFileExists path
        isDir <- fsDirectoryExists path
        hasDefault <- if isDir then fsFileExists (defaultNix path) else pure False
        pure $ case (isFile, isDir, hasDefault) of
            (True, _, _) -> Just path
            (_, True, True) -> Just (defaultNix path)
            (_, True, False) -> Just path
            _ -> Nothing

    defaultNix :: AbsPath -> AbsPath
    defaultNix dir = absPathUnsafe (dir.osPath </> ([osp|default.nix|] :: OsPath))

-- | Whether one name of a directory listing is one the segment can spell; a 'Hole' spells anything.
matchesSegment :: Segment -> Text -> Bool
matchesSegment = \case
    [] -> T.null
    Literal text : rest -> maybe False (matchesSegment rest) . T.stripPrefix text
    Hole : rest -> any (matchesSegment rest) . T.tails
