{- | Lowering every Nix file of the project into "Deslop.Module"s at once.

Each file is one module, named by "Frontend.Nix.ModuleName", with an
'ImportEdge' per thing "Frontend.Nix.Reference" finds it referring to:

* a path in the project, to each module it reaches ("Frontend.Nix.PathResolver");
  one that is not a @.nix@ file the walk found - a script, a secret, a
  directory - is a module too, with no edges of its own;
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
    ModuleName (..),
    ModuleRole (Source),
    canonicalName,
    moduleIdUnsafe,
    specifierUnsafe,
 )
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import FileSystem.Path (AbsPath (..), ProjectRelativePath, ProjectRoot (..), absPathUnsafe, decodeOsPath, portablePath, relativePathTo)
import Frontend.Nix.ModuleName (externalName, moduleNames, projectNames)
import Frontend.Nix.PathResolver (reachedBy)
import Frontend.Nix.Reference (Anchor (..), PathPattern, Reference (..), Referent (..), Scope, literalOf, referencesIn)
import Frontend.Nix.Unit (NixSource (..), NixUnit (..), Opacity (..), unitPath)
import Renderable (Renderable (render))
import System.OsPath (takeDirectory)

-- | What only the whole program shows, and the user should hear about.
data NixLowerError
    = -- | A module exists, but what it refers to is unknown.
      ReferencesNotRead ProjectRelativePath Opacity
    | -- | @foo.nix@ beside @foo\/default.nix@: one name for two modules.
      NameShared ModuleName [ProjectRelativePath]
    deriving stock (Show, Eq)

{- | Every unit as a 'Module', in the order given, then every module only a
reference reaches, in path order; and what the user should hear about them.
-}
lowerProject ::
    (RoFileSystem :> es) =>
    ProjectRoot -> Scope -> [NixUnit] -> Eff es ([Module], [NixLowerError])
lowerProject projectRoot scope units = do
    unitIds <- traverse (idOf . unitPath) units
    lowered <- zipWithM (lowerUnit projectRoot scope) unitIds units
    let modules = fmap fst lowered
        reachedOnly =
            Map.toList . (`Map.withoutKeys` Set.fromList unitIds) . Map.fromList $
                concatMap snd lowered
        assets = sortOn (.path) (fmap asset reachedOnly)
        everyModule = named (modules <> assets)
    pure (everyModule, notRead <> shared everyModule)
  where
    relative = relativePathTo projectRoot
    asset (moduleId, path) = Module {id = moduleId, names = moduleNames path, path, edges = [], role = Source}
    notRead = [ReferencesNotRead (relative path) why | Opaque path why _ <- units]

{- | Every module, and every edge into one, by the names 'projectNames' gives
it once the whole project is known: what a module is canonically called
depends on what lies beside it, so until then each is named by its path alone.

A module is renamed by its own path, never by its id: a symbolic link to a
@.nix@ file is a module of its own that shares the id of the file it links to.
An edge is renamed as the module of its id and the path it was written as, or,
reaching a module through a linked directory, as one of its id.
-}
named :: [Module] -> [Module]
named modules = fmap rename modules
  where
    byPath = projectNames (fmap (.path) modules)
    final m = Map.findWithDefault m.names m.path byPath
    -- Until now, a module's names and an edge's are those of the path alone.
    byIdAndPath = Map.fromList [((m.id, m.names), final m) | m <- modules]
    byId = Map.fromList [(m.id, final m) | m <- modules]
    rename m = m {names = final m, edges = fmap retarget m.edges}
    retarget edge = case edge.target of
        Resolved target names ->
            edge {target = Resolved target (fromMaybe names (Map.lookup (target, names) byIdAndPath <|> Map.lookup target byId))}
        _ -> edge

-- | Canonicalised, as every edge into it is, so the two cannot disagree.
idOf :: (RoFileSystem :> es) => AbsPath -> Eff es ModuleId
idOf = fmap (moduleIdUnsafe . decodeOsPath . (.osPath)) . fsMkAbsolute . (.osPath)

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

{- | Every name one module is canonically called and another is called at
all. Aliases may be shared; a canonical name may not, as a Problem id is
built from it.
-}
shared :: [Module] -> [NixLowerError]
shared modules =
    [ NameShared name (sort (ordNub claimants))
    | name <- ordNub (fmap canonicalName modules)
    , let claimants = Map.findWithDefault [] name claimedBy
    , length (ordNub claimants) > 1
    ]
  where
    claimedBy = Map.fromListWith (<>) [(name, [m.path]) | m <- modules, name <- ordNub (toList m.names)]

instance Renderable NixLowerError where
    render = \case
        ReferencesNotRead path why -> portablePath path <> " " <> reason why <> ", so what it refers to was not read."
        NameShared name paths ->
            T.intercalate ", " (fmap portablePath paths)
                <> " share the name "
                <> name.text
                <> ", so a Rule cannot tell them apart; rename or move all but one."
      where
        reason = \case
            NotUtf8 -> "is not UTF-8"
            Unlexable -> "could not be read as Nix"
