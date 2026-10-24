{-# LANGUAGE QuasiQuotes #-}

{- | The project a @.cabal@ file describes: one package and its components.

Read with Cabal's own parser, @Cabal-syntax@, so @common@ stanzas are resolved
the way @cabal@ resolves them. Conditional branches - @if os(linux)@,
@if flag(static)@ - are all taken at once: a source directory or a
@build-depends@ any configuration could use is one Deslop reads, since a Rule
should hold in every build, not only the one on this machine.

Only a single package is read. A root @cabal.project@ is accepted when it
names that same package and nothing else.
-}
module Frontend.Haskell.Cabal (
    HsConfig (..),
    Component (..),
    ComponentKind (..),
    MainIs (..),
    CabalError (..),
    Owner (..),
    OwnedAs (..),
    cabalFilesIn,
    loadHsConfig,
    qualifier,
    mainIsOf,
    ownerOf,
    sourceNamesOf,
) where

import Data.List.NonEmpty qualified as NE
import Control.Monad (foldM)
import Data.List qualified as L
import Data.Text qualified as T
import Deslop.Error (Warning (..))
import Distribution.Compat.NonEmptySet qualified as NES
import Distribution.Compiler (PerCompilerFlavor (..))
import Distribution.Fields (Field (..), FieldLine (..), Name (..), readFields)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (showPError, showPWarning)
import Distribution.Types.Benchmark qualified as C
import Distribution.Types.BenchmarkInterface qualified as C
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.Dependency (depLibraries, depPkgName)
import Distribution.Types.Executable qualified as C
import Distribution.Types.Library qualified as C
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.CondTree (CondTree, ignoreConditions)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.TestSuite qualified as C
import Distribution.Types.TestSuiteInterface qualified as C
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsDirectoryExists, fsFileExists, fsListDirectory, fsReadFile)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), absPathUnsafe, decodeOsPath, encodeOsPath, encodeOsPathString, withAbsBaseUnsafe)
import Frontend.Haskell.ModuleName (HsModuleName, hsModuleName)
import Frontend.Haskell.Preprocessor (Request, requestedByExtensions, requestedByOptions)
import Language.Haskell.Extension (Extension (..), KnownExtension (CPP))
import System.OsPath (OsPath, joinPath, osp, splitDirectories, splitExtension, takeExtension, takeFileName, (</>))

-- | The one package at the project root.
data HsConfig = HsConfig
    { package :: Text
    , components :: [Component]
    -- ^ In the order @cabal@ lists them: the library, sub-libraries,
    -- executables, test suites, benchmarks.
    }
    deriving stock (Show, Eq)

data Component = Component
    { kind :: ComponentKind
    , name :: Text
    -- ^ The main library is named after its package, as @cabal@ names it.
    , sourceDirs :: NonEmpty AbsPath
    -- ^ @hs-source-dirs@, @.@ when it names none.
    , libraries :: [Text]
    -- ^ The project's own libraries its @build-depends@ names, by component
    -- name: whose modules it may import besides its own.
    , preprocessors :: [Request]
    -- ^ What its @default-extensions@ and @ghc-options@ run over every one of
    -- its files. Not @other-extensions@, which enables nothing.
    }
    deriving stock (Show, Eq)

data ComponentKind
    = Library
    | Executable MainIs
    | TestSuite (Maybe MainIs)
    | -- | A test suite or benchmark of a type without a @main-is@, such as
      -- @detailed-0.9@, has no Main module.
      Benchmark (Maybe MainIs)
    deriving stock (Show, Eq)

-- | @main-is@, relative to one of the component's source directories.
newtype MainIs = MainIs OsPath
    deriving stock (Show, Eq)

mainIsOf :: ComponentKind -> Maybe MainIs
mainIsOf = \case
    Library -> Nothing
    Executable m -> Just m
    TestSuite m -> m
    Benchmark m -> m

{- | What every name of a component's modules is qualified with:
@lib\/deslop@, @exe\/deslop@, @test\/deslop-test@, @bench\/deslop-bench@.
-}
qualifier :: Component -> Text
qualifier c = prefix c.kind <> "/" <> c.name
  where
    prefix = \case
        Library -> "lib"
        Executable _ -> "exe"
        TestSuite _ -> "test"
        Benchmark _ -> "bench"

data CabalError
    = -- | Deslop reads one package, whose @.cabal@ file is at the root.
      NotOneCabalFile [AbsPath]
    | CabalUnreadable AbsPath [Text]
    | CabalProjectUnreadable AbsPath Text
    | -- | A @cabal.project@ naming packages other than the root one.
      MultiPackageProject AbsPath [Text]
    deriving stock (Show, Eq)

-- | The @*.cabal@ files directly in a directory; none in one that does not exist.
cabalFilesIn :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
cabalFilesIn dir = do
    entries <- fsDirectoryExists dir >>= bool (pure []) (fsListDirectory dir)
    filterM fsFileExists . sort . filter ((== [osp|.cabal|]) . takeExtension . (.osPath)) $ entries

loadHsConfig :: (RoFileSystem :> es) => ProjectRoot -> Eff es (Either CabalError (HsConfig, [Warning]))
loadHsConfig projectRoot =
    cabalFilesIn projectRoot.path >>= \case
        [cabalFile] ->
            checkCabalProject projectRoot cabalFile >>= \case
                Left err -> pure (Left err)
                Right () -> readPackage projectRoot cabalFile <$> fsReadFile cabalFile
        other -> pure . Left $ NotOneCabalFile other

readPackage :: ProjectRoot -> AbsPath -> ByteString -> Either CabalError (HsConfig, [Warning])
readPackage projectRoot cabalFile bytes = case runParseResult (parseGenericPackageDescription bytes) of
    (_, Left (_, errors)) -> Left . CabalUnreadable cabalFile . fmap (toText . showPError path) $ toList errors
    (warnings, Right gpd) ->
        Right
            ( configOf projectRoot gpd
            , Warning . toText . showPWarning path <$> warnings
            )
  where
    path = toString (decodeOsPath cabalFile.osPath)

{- | A root @cabal.project@ may only name the root package. Each @packages:@
entry - a directory, a @.cabal@ file, a glob of either, relative or absolute -
is resolved against the root as @cabal@ resolves it, and must name exactly the
root @.cabal@ file.
-}
checkCabalProject :: (RoFileSystem :> es) => ProjectRoot -> AbsPath -> Eff es (Either CabalError ())
checkCabalProject projectRoot cabalFile = do
    let projectFile = withAbsBaseUnsafe projectRoot.path [osp|cabal.project|]
    exists <- fsFileExists projectFile
    if not exists
        then pure (Right ())
        else fsReadFile projectFile >>= judge projectFile . readFields
  where
    judge projectFile = \case
        Left err -> pure . Left $ CabalProjectUnreadable projectFile (show err)
        Right fields ->
            filterM (fmap not . namesOnlyTheRoot) (packagesIn fields) <&> \case
                [] -> Right ()
                others -> Left (MultiPackageProject projectFile others)

    packagesIn fields =
        [ entry
        | Field (Name _ "packages") fieldLines <- fields
        , FieldLine _ text <- fieldLines
        , entry <- T.split (\c -> c == ',' || c == ' ' || c == '\t') (decodeUtf8 text)
        , not (T.null entry)
        ]

    namesOnlyTheRoot entry = (== [normalised cabalFile]) . sortNub . fmap normalised <$> cabalFilesNamedBy projectRoot entry
    normalised = splitDirectories . lexicallyNormal . (.osPath)

{- | The @.cabal@ files a @packages:@ entry names: those of a directory it
names, or the file itself; @*@ matches within one path segment.
-}
cabalFilesNamedBy :: (RoFileSystem :> es) => ProjectRoot -> Text -> Eff es [AbsPath]
cabalFilesNamedBy projectRoot entry = foldM step [start] segments >>= fmap concat . traverse packagesAt
  where
    (start, segments) = case T.stripPrefix "/" entry of
        Just absolute -> (absPathUnsafe [osp|/|], T.splitOn "/" absolute)
        Nothing -> (projectRoot.path, T.splitOn "/" entry)

    step paths segment
        | T.null segment || segment == "." = pure paths
        | T.any (`elem` ['*', '?']) segment = concat <$> traverse (matching segment) paths
        | otherwise = filterM exists (fmap (`withAbsBaseUnsafe` encodeOsPath segment) paths)

    matching pattern dir =
        fsDirectoryExists dir >>= \case
            False -> pure []
            True -> filter (globMatches (toString pattern) . toString . decodeOsPath . takeFileName . (.osPath)) <$> fsListDirectory dir

    exists path = (||) <$> fsFileExists path <*> fsDirectoryExists path

    packagesAt path =
        fsDirectoryExists path >>= \case
            True -> cabalFilesIn path
            False -> pure [path | takeExtension path.osPath == [osp|.cabal|]]

-- | A glob of @*@ (any run of characters) and @?@ (any one character).
globMatches :: String -> String -> Bool
globMatches pattern name = case (pattern, name) of
    ([], []) -> True
    ('*' : rest, _) -> any (globMatches rest) (tails name)
    ('?' : rest, _ : more) -> globMatches rest more
    (p : rest, n : more) -> p == n && globMatches rest more
    _ -> False

{- | Every component, each with all its conditional branches taken at once.
Not "Distribution.PackageDescription.Configuration.flattenPackageDescription",
which does the same but lists the components of each kind in reverse.
-}
configOf :: ProjectRoot -> GenericPackageDescription -> HsConfig
configOf projectRoot gpd =
    HsConfig
        { package = packageName
        , components =
            fmap library (mainLibrary <> subLibraries)
                <> [executable (unconditional t) {C.exeName = n} | (n, t) <- gpd.condExecutables]
                <> [testSuite (unconditional t) {C.testName = n} | (n, t) <- gpd.condTestSuites]
                <> [benchmark (unconditional t) {C.benchmarkName = n} | (n, t) <- gpd.condBenchmarks]
        }
  where
    packageName = toText . unPackageName . pkgName $ gpd.packageDescription.package
    mainLibrary = [(unconditional t) {C.libName = LMainLibName} | t <- maybeToList gpd.condLibrary]
    subLibraries = [(unconditional t) {C.libName = LSubLibName n} | (n, t) <- gpd.condSubLibraries]
    subLibraryNames = [toText (unUnqualComponentName n) | (n, _) <- gpd.condSubLibraries]

    unconditional :: (Semigroup a, Semigroup c) => CondTree v c a -> a
    unconditional = fst . ignoreConditions

    library lib = component Library (libraryName lib.libName) lib.libBuildInfo
    executable exe = component (Executable (mainIs exe.modulePath)) (unqual exe.exeName) exe.buildInfo
    testSuite t = component (TestSuite (testMain t.testInterface)) (unqual t.testName) t.testBuildInfo
    benchmark b = component (Benchmark (benchMain b.benchmarkInterface)) (unqual b.benchmarkName) b.benchmarkBuildInfo

    testMain = \case
        C.TestSuiteExeV10 _ path -> Just (mainIs path)
        _ -> Nothing
    benchMain = \case
        C.BenchmarkExeV10 _ path -> Just (mainIs path)
        _ -> Nothing

    mainIs = MainIs . encodeOsPathString
    unqual = toText . unUnqualComponentName
    libraryName = \case
        LMainLibName -> packageName
        LSubLibName n -> unqual n

    component kind name bi =
        let c =
                Component
                    { kind
                    , name
                    , sourceDirs = sourceDirsOf bi
                    , libraries = filter (\lib -> kind /= Library || lib /= name) (librariesOf bi)
                    , preprocessors = []
                    }
            label = qualifier c
         in c {preprocessors = preprocessorsOf (T.replace "/" ":" label) bi}

    sourceDirsOf bi =
        fmap (absPathUnsafe . lexicallyNormal . (projectRoot.path.osPath </>) . encodeOsPathString)
            . fromMaybe ("." :| [])
            . nonEmpty
            . fmap getSymbolicPath
            $ bi.hsSourceDirs

    -- @build-depends: deslop@ is the main library, @deslop:sub@ or the
    -- older bare @sub@ a sub-library.
    librariesOf bi = ordNub (concatMap projectLibraries bi.targetBuildDepends)
    projectLibraries dep
        | depName == packageName = libraryName <$> NES.toList (depLibraries dep)
        | depName `elem` subLibraryNames = [depName]
        | otherwise = []
      where
        depName = toText . unPackageName . depPkgName $ dep

    preprocessorsOf label bi =
        -- other-extensions only declares what some file's own pragma enables.
        requestedByExtensions label (take 1 ["CPP" | EnableExtension CPP <- bi.defaultExtensions])
            <> requestedByOptions label (toText <$> ghcOptions bi)

    ghcOptions bi = let PerCompilerFlavor ghc _ = bi.options in ghc

-- | @a/./b/../c@ as @a/c@, without asking the filesystem.
lexicallyNormal :: OsPath -> OsPath
lexicallyNormal = joinPath . reverse . foldl' step [] . splitDirectories
  where
    step acc segment
        | segment == [osp|.|] = acc
        | segment == [osp|..|], parent : rest <- acc, parent /= [osp|..|], not (isRoot parent) = rest
        | otherwise = segment : acc
    isRoot segment = takeFileName segment /= segment

-- | Which component compiles a file, and what it calls it.
data Owner = Owner
    { component :: Component
    , as :: OwnedAs
    }
    deriving stock (Show, Eq)

data OwnedAs
    = -- | The component's @main-is@, which is @Main@ whatever its file is called.
      MainModule
    | -- | Named by its path below the source directory, as GHC looks it up.
      PathNamed HsModuleName
    deriving stock (Show, Eq)

{- | The component that compiles a file.

A component's @main-is@ is its own, whichever directories overlap. Otherwise
the deepest source directory containing the file wins, and of components
sharing that directory, the first in the @.cabal@ file: deterministic, and
what makes @src/@ inside a @.@ source directory belong to the component that
names @src@.
-}
ownerOf :: [Component] -> AbsPath -> Maybe Owner
ownerOf components file = asMain <|> uncurry Owner . second PathNamed <$> listToMaybe (sourceNamesOf components file)
  where
    fileSegments = splitDirectories file.osPath

    asMain =
        listToMaybe
            [ Owner c MainModule
            | c <- components
            , MainIs m <- maybeToList (mainIsOf c.kind)
            , dir <- toList c.sourceDirs
            , splitDirectories (lexicallyNormal (dir.osPath </> m)) == fileSegments
            ]

{- | Every component with a source directory holding the file, and the name
GHC looks the file up by from that directory: deepest directory first, then
in @.cabal@ order. GHC compiles the file into each of them, so each may import
it, though only the first owns it.
-}
sourceNamesOf :: [Component] -> AbsPath -> [(Component, HsModuleName)]
sourceNamesOf components file =
    fmap snd
        . sortOn (Down . fst)
        $ [ (length dirSegments, (c, nameBelow rest))
          | c <- components
          , dir <- toList c.sourceDirs
          , let dirSegments = splitDirectories dir.osPath
          , Just rest <- [nonEmpty =<< L.stripPrefix dirSegments (splitDirectories file.osPath)]
          ]
  where
    nameBelow segments =
        hsModuleName . fmap decodeOsPath $
            NE.prependList (NE.init segments) (fst (splitExtension (NE.last segments)) :| [])
