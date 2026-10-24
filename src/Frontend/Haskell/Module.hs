{- | Lowering every Haskell file of the project into "Deslop.Module"s at once.

Haskell cannot name a dependency one file at a time: @import Deslop.Module@
means whichever file of the importing component - or of a library it depends
on - is called @Deslop.Module@. So this first names every file, then indexes
each component's modules, then resolves every import against the index of the
component that compiles it.

Names, canonical first (see @docs\/adr\/0021@):

* a module is @Deslop\/Module@, aliased by its component as
  @lib\/deslop\/Deslop\/Module@;
* a component's @main-is@ is @exe\/deslop\/Main@, aliased @Main@;
* modules of two components sharing a bare name, such as a library's @Utils@
  and a test suite's, are each canonically their component-qualified name.
-}
module Frontend.Haskell.Module (
    HsLowerError (..),
    lowerProject,
) where

import Data.Map.Strict qualified as Map
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleId,
    ModuleName (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import FileSystem.Path (AbsPath (..), ProjectRelativePath (..), ProjectRoot, decodeOsPath, portablePath, relativePathTo)
import Frontend.Haskell.Cabal (Component (..), ComponentKind (..), HsConfig (..), OwnedAs (..), Owner (..), ownerOf, qualifier)
import Frontend.Haskell.CST (Header (..), HsProgram (..), HsSource (..), Import (..), ImportDecl (..), ModuleExport (..), ModuleHeader (..))
import Frontend.Haskell.HspecDiscover (DiscoveredSpec (..), discoveredSpecs)
import Frontend.Haskell.ModuleName (HsModuleName, dotted, hsModuleName, mainModule, slashed)
import Frontend.Haskell.Preprocessor (Preprocessor (..), Request (..), RequestedBy (..), requestedInFile)
import Frontend.Haskell.Unit (HsUnit (..), Opacity (..), unitPath)
import Renderable (Renderable (render))
import System.OsPath (dropExtension, splitDirectories)

-- | What only the whole program shows, and the user should hear about.
data HsLowerError
    = -- | A module exists, but what it imports is unknown.
      ImportsNotRead ProjectRelativePath Opacity
    | -- | @module A.B@ in a file GHC would look up as @A.C@: Deslop goes by the path.
      HeaderDisagrees ProjectRelativePath HsModuleName HsModuleName
    | -- | Files of different components sharing one bare name, each named by
      -- its component instead.
      NameShared ModuleName [ProjectRelativePath]
    deriving stock (Show, Eq)

{- | Every unit as a 'Module', in the order given, and what the user should
hear about them.
-}
lowerProject ::
    (RoFileSystem :> es) =>
    ProjectRoot -> HsConfig -> [HsUnit] -> Eff es ([Module], [HsLowerError])
lowerProject projectRoot cfg units = do
    -- Canonicalised once per file, and used both for the module and for every
    -- edge into it, so the two can never disagree.
    ids <- traverse (fmap (moduleIdUnsafe . decodeOsPath . (.osPath)) . fsMkAbsolute . (.osPath) . unitPath) units
    pure $ lowerNamed projectRoot cfg (zipWith (named projectRoot cfg) ids units)

-- | A unit, with who it is and what compiles it.
data Named = Named
    { id :: ModuleId
    , unit :: HsUnit
    , owner :: Maybe Owner
    , bare :: HsModuleName
    -- ^ What GHC calls it: @Main@ for a @main-is@.
    }

named :: ProjectRoot -> HsConfig -> ModuleId -> HsUnit -> Named
named projectRoot cfg moduleId unit =
    Named {id = moduleId, unit, owner, bare}
  where
    path = unitPath unit
    owner = ownerOf cfg path
    bare = case owner of
        Just (Owner _ MainModule) -> mainModule
        Just (Owner _ (PathNamed n)) -> n
        -- Never discovered: every walked file sits in a source directory.
        -- Still a module, named by its path from the root.
        Nothing -> hsModuleName . pathSegments . relativePathTo projectRoot $ path
    pathSegments rel = fromMaybe ("Main" :| []) . nonEmpty . fmap decodeOsPath . splitDirectories . dropExtension $ rel.osPath

lowerNamed :: ProjectRoot -> HsConfig -> [Named] -> ([Module], [HsLowerError])
lowerNamed projectRoot cfg entries = (fmap moduleOf entries, notRead <> disagreements <> shared)
  where
    relPath = relativePathTo projectRoot . unitPath . (.unit)

    (namesOf, shared) = nameAll relPath entries
    -- What every edge into the module carries: its id, and every name it has.
    reaching entry = Resolved entry.id (namesOf entry)

    -- Each component's modules by bare name; a main-is is imported by no one.
    ownIndex :: Map Text (Map HsModuleName EdgeTarget)
    ownIndex =
        Map.fromListWith
            (flip Map.union)
            [ (qualifier c, Map.singleton n (reaching entry))
            | entry <- entries
            , Just (Owner c (PathNamed n)) <- [entry.owner]
            ]
    indexOf c = Map.findWithDefault mempty (qualifier c) ownIndex
    -- Its own modules first, then those of the project libraries it depends on.
    visibleTo c = Map.unions (indexOf c : [indexOf lib | lib <- cfg.components, lib.kind == Library, lib.name `elem` c.libraries])

    allPaths = unitPath . (.unit) <$> entries
    byPath = Map.fromList [(unitPath entry.unit, entry) | entry <- entries]

    moduleOf entry =
        Module
            { id = entry.id
            , names = namesOf entry
            , path = relPath entry
            , edges = case entry.unit of
                Readable program -> edgesOf entry program
                Opaque {} -> []
            }

    edgesOf entry program =
        let file = relPath entry
            component = (.component) <$> entry.owner
            resolveImport = resolve component
            importEdges = [importEdge file i (resolveImport i.decl) | i <- program.source.imports]
         in importEdges
                <> reExportEdges file resolveImport program.source
                <> hspecEdges file program

    resolve :: Maybe Component -> ImportDecl -> EdgeTarget
    resolve component decl =
        fromMaybe (External (slashed decl.moduleName)) $ do
            c <- component
            index <- case decl.package of
                Nothing -> Just (visibleTo c)
                Just "this" -> Just (indexOf c)
                Just p | p == cfg.package -> Just (visibleTo c)
                Just _ -> Nothing
            Map.lookup decl.moduleName index

    hspecEdges file program =
        [ DependencyEdge
            { specifier = specifierUnsafe (dotted spec.name)
            , target = maybe (External (slashed spec.name)) reaching (Map.lookup spec.path byPath)
            , kind = ImportEdge
            , location = Location {file, line, code}
            }
        | Request HspecDiscover (ByPragma line code) <- take 1 (filter isHspec (requestedInFile program.source.leading))
        , spec <- discoveredSpecs program.path allPaths
        ]
    isHspec r = r.preprocessor == HspecDiscover

    notRead = [ImportsNotRead (relPath entry) why | entry <- entries, Opaque _ why _ <- [entry.unit]]

    disagreements =
        [ HeaderDisagrees (relPath entry) h.name n
        | entry <- entries
        , Readable program <- [entry.unit]
        , Declared h <- [program.source.header]
        , Just (Owner _ (PathNamed n)) <- [entry.owner]
        , h.name /= n
        ]

importEdge :: ProjectRelativePath -> Import -> EdgeTarget -> DependencyEdge
importEdge file i target =
    DependencyEdge
        { specifier = specifierUnsafe (dotted i.decl.moduleName)
        , target
        , kind = ImportEdge
        , location = Location {file, line = i.line, code = render i.own}
        }

{- | @module X@ in the export list re-exports what every import named @X@ - by
its alias, or by its own name when it has none - brought into scope. The
module's own name is no edge: @module M@ in @M@'s list exports @M@'s own
declarations.
-}
reExportEdges :: ProjectRelativePath -> (ImportDecl -> EdgeTarget) -> HsSource -> [DependencyEdge]
reExportEdges file resolveImport source = case source.header of
    ImplicitMain -> []
    Declared h ->
        [ DependencyEdge
            { specifier = specifierUnsafe (dotted export.name)
            , target = resolveImport i.decl
            , kind = ReExportEdge
            , location = Location {file, line = export.line, code = export.code}
            }
        | export <- h.reExports
        , export.name /= h.name
        , i <- source.imports
        , fromMaybe i.decl.moduleName i.decl.alias == export.name
        ]

{- | Every module's names, canonical first, and the bare names that more than
one module claimed.
-}
nameAll :: (Named -> ProjectRelativePath) -> [Named] -> (Named -> NonEmpty ModuleName, [HsLowerError])
nameAll relPath entries = (namesOf, clashes)
  where
    claimants = Map.fromListWith (flip (<>)) [(canonical entry, [entry]) | entry <- entries]
    isShared entry = maybe False ((> 1) . length) (Map.lookup (canonical entry) claimants)

    namesOf entry = bool defaultNames qualifiedFirst (isShared entry) entry

    canonical = head . defaultNames

    defaultNames entry = case entry.owner of
        Just (Owner c MainModule) -> qualified c entry.bare :| [slashed entry.bare]
        Just (Owner c (PathNamed _)) -> slashed entry.bare :| [qualified c entry.bare]
        Nothing -> slashed entry.bare :| []

    qualifiedFirst entry = case entry.owner of
        Just (Owner c _) -> qualified c entry.bare :| [slashed entry.bare]
        Nothing -> defaultNames entry

    qualified c n = moduleNameUnsafe (qualifier c <> "/" <> (slashed n).text)

    clashes =
        [ NameShared name (sort (fmap relPath claimed))
        | (name, claimed@(_ : _ : _)) <- Map.toList claimants
        ]

instance Renderable HsLowerError where
    render = \case
        ImportsNotRead path why -> portablePath path <> " " <> reason why <> ", so its imports were not read."
        HeaderDisagrees path declared expected ->
            portablePath path
                <> " declares module "
                <> dotted declared
                <> ", but GHC looks it up by its path as "
                <> dotted expected
                <> "; Deslop names it "
                <> (slashed expected).text
                <> "."
        NameShared name paths ->
            unwords (fmap portablePath paths)
                <> " are all called "
                <> name.text
                <> ", so each is named by its component instead."
      where
        reason = \case
            NotUtf8 -> "is not UTF-8"
            Unparseable -> "could not be parsed"
            Preprocessed request -> preprocessedBy request.preprocessor <> requestedBy request.by
        preprocessedBy = \case
            Cpp -> "is preprocessed by CPP"
            HspecDiscover -> "is preprocessed by hspec-discover"
            Custom program -> "is preprocessed by " <> program
        requestedBy = \case
            ByPragma line _ -> " (line " <> show line <> ")"
            ByComponent component -> " (enabled for " <> component <> ")"
