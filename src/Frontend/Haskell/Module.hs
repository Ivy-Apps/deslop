{- | Lowering every Haskell file of the project into "Deslop.Module"s at once.

Haskell cannot name a dependency one file at a time: @import Deslop.Module@
means whichever file of the importing component - or of a library it depends
on - is called @Deslop.Module@. So this first names every file, then indexes
each component's modules, then resolves every import against the index of the
component that compiles it.

Names, canonical first (see @docs\/adr\/0021@): a module is canonically
named by its component, @lib\/deslop\/Deslop\/Module@, and aliased by what GHC
calls it, @Deslop\/Module@. A component's @main-is@ is @exe\/deslop\/Main@,
aliased @Main@. Every canonical name starts with its component, so a
library's @Utils@ and a test suite's never share one.
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
import Frontend.Haskell.CST (Header (..), HsProgram (..), HsSource (..), Import (..), ImportDecl (..), ModuleExport (..), ModuleHeader (..), Qualification (..))
import Frontend.Haskell.Cabal (Component (..), ComponentKind (..), HsConfig (..), OwnedAs (..), Owner (..), ownerOf, qualifier, sourceNamesOf)
import Frontend.Haskell.HspecDiscover (DiscoveredSpec (..), discoveredSpecs)
import Frontend.Haskell.ModuleName (HsModuleName, dotted, hsModuleName, mainModule, slashed)
import Frontend.Haskell.ModuleRole (moduleRole)
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
    | {- | Files of one component claiming one name, from two of its source
      directories: GHC rejects the import as ambiguous.
      -}
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
    pure $ lowerNamed projectRoot cfg (zipWith (named projectRoot cfg.components) ids units)

-- | A unit, with who it is and what compiles it.
data Named = Named
    { id :: ModuleId
    , unit :: HsUnit
    , owner :: Maybe Owner
    , bare :: HsModuleName
    -- ^ What GHC calls it: @Main@ for a @main-is@.
    }

named :: ProjectRoot -> [Component] -> ModuleId -> HsUnit -> Named
named projectRoot components moduleId unit =
    Named {id = moduleId, unit, owner, bare}
  where
    path = unitPath unit
    owner = ownerOf components path
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

    -- What each component compiles, by the name it looks each file up by:
    -- every file in its source directories, including those another
    -- component owns, reaching the owner's module. Its own main-is is
    -- imported by no one.
    ownIndex :: Map Text (Map HsModuleName EdgeTarget)
    ownIndex =
        Map.fromListWith
            (flip Map.union)
            [ (qualifier c, Map.singleton n (reaching entry))
            | entry <- entries
            , (c, n) <- sourceNamesOf cfg.components (unitPath entry.unit)
            , not (isMainOf c entry)
            ]
    isMainOf c entry = case entry.owner of
        Just (Owner owner MainModule) -> qualifier owner == qualifier c
        _ -> False
    indexOf c = Map.findWithDefault mempty (qualifier c) ownIndex
    librariesOf c = [indexOf lib | lib <- cfg.components, lib.kind == Library, lib.name `elem` c.libraries]
    -- Its own modules first, then those of the project libraries it depends on.
    visibleTo c = Map.unions (indexOf c : librariesOf c)
    -- What @import "p" M@ searches, p being this package: its libraries only.
    packageLibraries c = Map.unions ([indexOf c | c.kind == Library] <> librariesOf c)

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
            , role = moduleRole ((.component.kind) <$> entry.owner) $ case entry.unit of
                Readable program -> (.decl.moduleName) <$> program.source.imports
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
                Just p | p == cfg.package -> Just (packageLibraries c)
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

{- | @module X@ in the export list re-exports what every unqualified import
named @X@ - by its alias, or by its own name when it has none - brought into
scope (Haskell 2010, 5.2). A qualified import brings nothing unqualified into
scope, so it is exported by no @module X@. @module M@ in @M@'s own list
exports @M@'s own declarations, and also every import aliased @as M@.
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
        , i <- source.imports
        , i.decl.qualification == Unqualified
        , fromMaybe i.decl.moduleName i.decl.alias == export.name
        ]

{- | Every module's names, canonical first, and the canonical names more than
one module claimed - which only two source directories of one component can
cause.
-}
nameAll :: (Named -> ProjectRelativePath) -> [Named] -> (Named -> NonEmpty ModuleName, [HsLowerError])
nameAll relPath entries = (namesOf, clashes)
  where
    claimants = Map.fromListWith (flip (<>)) [(head (namesOf entry), [entry]) | entry <- entries]

    namesOf entry = case entry.owner of
        Just (Owner c _) -> qualified c entry.bare :| [slashed entry.bare]
        Nothing -> slashed entry.bare :| []

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
                <> " are all "
                <> name.text
                <> ", which GHC cannot tell apart; rename or move all but one."
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
