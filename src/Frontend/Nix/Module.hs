{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE. Lowering scanned Nix files into "Deslop.Module": one Module per
@.nix@ file, named by its project-relative path without @.nix@, and a
@default.nix@ also by its directory - @import ./lib@ means @./lib/default.nix@.
-}
module Frontend.Nix.Module (lowerNix, resolvePath) where

import Data.Text qualified as T
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleName,
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import Effectful
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (RoFileSystem, fsDirectoryExists, fsFileExists, fsMkAbsolute)
import FileSystem.Path (
    AbsPath (..),
    ProjectRoot (..),
    absPathUnsafe,
    decodeOsPath,
    encodeOsPath,
    portablePath,
    relativePathTo,
 )
import Frontend.Nix.Parser (NixRef (..), NixUnit (..))
import System.OsPath (osp, takeDirectory, takeFileName, (</>))

lowerNix :: (Reader ProjectRoot :> es, RoFileSystem :> es) => NixUnit -> Eff es Module
lowerNix unit = do
    root <- canonicalRoot
    canonical <- fsMkAbsolute unit.path.osPath
    let relPath = relativePathTo root unit.path
    edges <- traverse (edgeOf root relPath) unit.refs
    pure
        Module
            { id = moduleIdUnsafe . decodeOsPath $ canonical.osPath
            , names = namesOf root canonical
            , path = relPath
            , edges = edges
            }
  where
    edgeOf root file ref = do
        target <- targetOf root ref
        pure
            DependencyEdge
                { specifier = specifierOf ref
                , target = target
                , kind = ImportEdge
                , location = Location {file = file, line = ref.line, code = ref.code}
                }
    targetOf root = \case
        PathRef {written} ->
            resolvePath unit.path written <&> \case
                Just p -> Resolved (moduleIdUnsafe . decodeOsPath $ p.osPath) (namesOf root p)
                Nothing -> External (specifierUnsafe written)
        LookupRef {written} -> pure . External . specifierUnsafe $ written
        -- The spike's dilemma: an edge we know exists but cannot name has
        -- nowhere to go but External, which also means "a third-party package".
        DynamicRef {code} -> pure . External . specifierUnsafe $ code
    specifierOf = \case
        PathRef {written} -> specifierUnsafe written
        LookupRef {written} -> specifierUnsafe written
        DynamicRef {code} -> specifierUnsafe code

canonicalRoot :: (Reader ProjectRoot :> es, RoFileSystem :> es) => Eff es ProjectRoot
canonicalRoot = ask @ProjectRoot >>= fmap ProjectRoot . fsMkAbsolute . (.path.osPath)

{- | What a literal path written in the importer names: the file itself, or
a directory's @default.nix@. Canonical, or nothing when neither exists.
-}
resolvePath :: (RoFileSystem :> es) => AbsPath -> Text -> Eff es (Maybe AbsPath)
resolvePath importer written = do
    let target = takeDirectory importer.osPath </> encodeOsPath written
    isDir <- fsDirectoryExists (absPathUnsafe target)
    let file = bool target (target </> [osp|default.nix|]) isDir
    exists <- fsFileExists (absPathUnsafe file)
    if exists then Just <$> fsMkAbsolute file else pure Nothing

namesOf :: ProjectRoot -> AbsPath -> NonEmpty ModuleName
namesOf root p
    | takeFileName p.osPath == [osp|default.nix|] = moduleNameUnsafe dirName :| [moduleNameUnsafe bare]
    | otherwise = moduleNameUnsafe bare :| []
  where
    rel = portablePath . relativePathTo root $ p
    bare = "/" <> fromMaybe rel (T.stripSuffix ".nix" rel)
    dirName = fromMaybe "/" . (\d -> d <$ guard (d /= "")) . fromMaybe bare $ T.stripSuffix "/default" bare
