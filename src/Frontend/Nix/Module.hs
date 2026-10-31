{- | Lowering every file of a Nix project into "Deslop.Module"s at once.

Each file is one module, named by "Frontend.Nix.ModuleName". One that is not
Nix is never read, and has no edges of its own. A @.nix@ file has an
'ImportEdge' per thing "Frontend.Nix.Reference" finds it referring to:

* a path in the project, to each module it reaches ("Frontend.Nix.PathResolver");
  one the walk did not find - a gitignored file, a directory - is a module too,
  with no edges of its own;
* a flake input, nixpkgs' NixOS modules or a search path entry, to an
  'External' module: @inputs\/\<name\>\/\<attr\>...@,
  @inputs\/nixpkgs\/nixos\/modules\/...@ and @nixpkgs\/...@;
* a path only evaluation names, or a path to nothing in the project, to
  'Unresolved', which a Rule reports as one it cannot judge.

A reference to the module itself is no dependency, and two references
reaching one target with one spelling are one edge.
-}
module Frontend.Nix.Module (
    NixLowerError (..),
    lowerProject,
) where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleId,
    ModuleRole (Source),
    moduleIdUnsafe,
    specifierUnsafe,
 )
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import FileSystem.Path (AbsPath (..), ProjectRelativePath, ProjectRoot (..), absPathUnsafe, decodeOsPath, portablePath, relativePathTo)
import Frontend.Nix.ModuleName (externalName, moduleNames)
import Frontend.Nix.PathResolver (reachedBy)
import Frontend.Nix.Reference (Anchor (..), PathPattern, Reference (..), Referent (..), Scope, literalOf, referencesIn)
import Frontend.Nix.Unit (NixSource (..), NixUnit (..), Opacity (..), unitPath)
import Renderable (Renderable (render))
import System.OsPath (takeDirectory)

-- | A module exists, but what it refers to is unknown.
data NixLowerError = ReferencesNotRead ProjectRelativePath Opacity
    deriving stock (Show, Eq)

{- | Every unit as a 'Module', in the order given, then every other file of
the project and every module only a reference reaches, in path order; and what
the user should hear about them.
-}
lowerProject ::
    (RoFileSystem :> es) =>
    ProjectRoot -> Scope -> [AbsPath] -> [NixUnit] -> Eff es ([Module], [NixLowerError])
lowerProject projectRoot scope otherFiles units = do
    unitIds <- traverse (idOf . unitPath) units
    lowered <- zipWithM (lowerUnit projectRoot scope) unitIds units
    let reached = Map.fromList (concatMap snd lowered)
    others <- oneModulePerId projectRoot (Set.fromList unitIds <> Map.keysSet reached) otherFiles
    let modules = fmap fst lowered
        reachedOnly = Map.toList (reached `Map.withoutKeys` Set.fromList (unitIds <> fmap fst others))
        assets = sortOn (.path) (fmap asset (others <> reachedOnly))
    pure (settleEdgeNames (modules <> assets), notRead)
  where
    asset (moduleId, path) = Module {id = moduleId, names = moduleNames path, path, edges = [], role = Source}
    notRead = [ReferencesNotRead (relativePathTo projectRoot path) why | Opaque path why _ <- units]

{- | The files that are not units, by id. A symbolic link shares the id of the
file it links to, so it gives way to that file whatever the walk order, or to
what already claims that id: a unit, or a path a reference reaches. A link
nothing else speaks for - out of the project, or to an ignored file no path
reaches - stands for the file it links to; of several, the first by path does.
-}
oneModulePerId ::
    (RoFileSystem :> es) =>
    ProjectRoot -> Set ModuleId -> [AbsPath] -> Eff es [(ModuleId, ProjectRelativePath)]
oneModulePerId projectRoot claimed files = do
    realRoot <- ProjectRoot <$> fsMkAbsolute projectRoot.path.osPath
    found <- forM files $ \file -> do
        real <- fsMkAbsolute file.osPath
        let path = relativePathTo projectRoot file
            spelling = if relativePathTo realRoot real == path then TheFile else ALink
        pure (spelling, idAt real, path)
    pure
        [ (moduleId, path)
        | (spelling, moduleId, path) <- nubOrdOn (\(_, moduleId, _) -> moduleId) (sort found)
        , spelling == TheFile || moduleId `Set.notMember` claimed
        ]

-- | How a file spells its id's path. A 'TheFile' sorts before the links to it.
data Spelling = TheFile | ALink
    deriving stock (Eq, Ord)

{- | Every edge into a module by that module's names. An edge is named by the
path it reached; through a linked directory that is no module's path, so it
takes the names of the module of its id.

A @.nix@ symbolic link to a file is a module of its own that shares the id of
the file it links to, so an edge that does name a module's path keeps that name.
-}
settleEdgeNames :: [Module] -> [Module]
settleEdgeNames modules = fmap (\m -> m {edges = fmap settle m.edges}) modules
  where
    known = Set.fromList [(m.id, m.names) | m <- modules]
    byId = Map.fromList [(m.id, m.names) | m <- modules]
    settle edge = case edge.target of
        Resolved target names -> edge {target = Resolved target (settled target names)}
        External _ -> edge
        Unresolved _ -> edge
    settled target names
        | (target, names) `Set.member` known = names
        | otherwise = Map.findWithDefault names target byId

-- | Canonicalised, as every edge into it is, so the two cannot disagree.
idOf :: (RoFileSystem :> es) => AbsPath -> Eff es ModuleId
idOf = fmap idAt . fsMkAbsolute . (.osPath)

idAt :: AbsPath -> ModuleId
idAt = moduleIdUnsafe . decodeOsPath . (.osPath)

{- | One unit's module, and every module in the project its edges reach,
by id and path.
-}
lowerUnit ::
    (RoFileSystem :> es) =>
    ProjectRoot ->
    Scope ->
    ModuleId ->
    NixUnit ->
    Eff es (Module, [(ModuleId, ProjectRelativePath)])
lowerUnit projectRoot scope moduleId unit = do
    resolved <- case unit of
        Readable source -> concat <$> traverse (resolve source (render source.lexemes)) (referencesIn scope source.lexemes)
        Opaque {} -> pure []
    let others = [(edge, reached) | (edge, reached) <- resolved, fmap fst reached.inProject /= Just moduleId]
        edges = nubOrdOn (\e -> (e.specifier, e.target)) [edge reached.edgeTarget | (edge, reached) <- others]
    pure (Module {id = moduleId, names = moduleNames file, path = file, edges, role = Source}, mapMaybe ((.inProject) . snd) others)
  where
    file = relativePathTo projectRoot (unitPath unit)

    -- Each target of the reference, with the edge reaching it.
    resolve source text reference = do
        let edge target =
                DependencyEdge
                    { specifier = specifierUnsafe reference.written
                    , target
                    , kind = ImportEdge
                    , location = locate file text reference.offset
                    }
        fmap (edge,) <$> reachedFrom projectRoot (absPathUnsafe (takeDirectory source.path.osPath)) reference

-- | What a reference reaches, and - for a module of the project - its id and path.
data Reached = Reached
    { edgeTarget :: EdgeTarget
    , inProject :: Maybe (ModuleId, ProjectRelativePath)
    }

-- | Everything one reference, written in this directory, reaches.
reachedFrom ::
    (RoFileSystem :> es) =>
    ProjectRoot ->
    AbsPath ->
    Reference ->
    Eff es [Reached]
reachedFrom projectRoot directory reference = case reference.referent of
    LocalPath Elsewhere _ -> pure [unresolved]
    LocalPath FromFile pattern -> local directory pattern
    LocalPath FromRoot pattern -> local projectRoot.path pattern
    FlakeInput name attrs -> pure [external ("inputs" :| name : attrs)]
    NixosModule pattern -> pure [external ("inputs" :| ["nixpkgs", "nixos", "modules"] <> literalPrefix pattern)]
    SearchPathEntry entry -> pure [maybe unresolved external (nonEmpty entry)]
    Computed -> pure [unresolved]
  where
    unresolved = Reached (Unresolved (specifierUnsafe reference.written)) Nothing
    external = (`Reached` Nothing) . External . externalName
    local base pattern =
        reachedBy projectRoot base pattern >>= \case
            [] -> pure [unresolved]
            paths -> traverse inProject paths
    inProject path = do
        target <- idOf path
        let rel = relativePathTo projectRoot path
        pure (Reached (Resolved target (moduleNames rel)) (Just (target, rel)))

-- | The segments of a pattern up to the first computed one.
literalPrefix :: PathPattern -> [Text]
literalPrefix = catMaybes . takeWhile isJust . fmap literalOf

-- | The 1-based line an offset is on, and that line, trimmed, as the statement to quote.
locate :: ProjectRelativePath -> Text -> Int -> Location
locate file source offset = Location {file, line, code}
  where
    before = T.take offset source
    line = 1 + T.count "\n" before
    code = T.strip (T.takeWhileEnd (/= '\n') before <> T.takeWhile (/= '\n') (T.drop offset source))

instance Renderable NixLowerError where
    render = \case
        ReferencesNotRead path why -> portablePath path <> " " <> reason why <> ", so what it refers to was not read."
      where
        reason = \case
            NotUtf8 -> "is not UTF-8"
            Unlexable -> "could not be read as Nix"
