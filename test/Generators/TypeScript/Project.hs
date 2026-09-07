{- | Generating a whole TypeScript project, so that a property can say the same
architecture spelled two different ways is the same architecture.

A 'Project' says which files exist and which files each one depends on. How
those dependencies are /written/ is a separate choice - a 'Spelling' - and the
point of separating the two is that respelling a project must never change what
it means. Every way of naming one module is a fact about that module, not a
different module.

Nothing here touches the disk: the file set is handed to the in-memory
'Doubles.FileSystem' double, which is what resolution asks about.
-}
module Generators.TypeScript.Project (
    Project (..),
    ProjectModule (..),
    Spelling (..),
    Naming (..),
    genProject,
    asImports,
    asRelative,
    asReExports,
    indexForm,
    directoryForm,
    primaryAlias,
    secondaryAlias,
    projectConfig,
    projectRoot,
    projectFiles,
    renderProject,
    projectAsts,
    reachability,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deslop.CodeGraph (GraphKey (..), ModuleRef (..), buildModuleGraph, reachableFrom)
import Deslop.Module (Module (..), ModuleName (..), canonicalName)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff, runPureEff)
import Effectful.Reader.Static (runReader)
import FileSystem.Path (AbsPath, ProjectRoot (..), absPathUnsafe, encodeOsPath)
import Fixtures.TypeScript.Config (mkMapping)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (OsPath, osp)
import TypeScript.Config (Pattern (..), TsConfig (..))
import TypeScript.CST (TsProgram (..))
import TypeScript.Module (parseModule)
import TypeScript.Parser (TsFile (..), parseTs)

{- | A module named by where it lives, relative to @src\/@ and without the
extension. A name ending in @\/index@ is a barrel.
-}
data ProjectModule = ProjectModule
    { name :: Text
    , deps :: [Text]
    }
    deriving stock (Show, Eq)

newtype Project = Project
    { modules :: [ProjectModule]
    }
    deriving stock (Show, Eq)

-- | How one project is written down. Respelling must not change its meaning.
data Spelling = Spelling
    { reExport :: Bool
    -- ^ Write dependencies as @export ... from@ rather than @import@.
    , directory :: Bool
    -- ^ Name a barrel by its directory rather than by its index file.
    , naming :: Naming
    -- ^ How a dependency is named.
    }
    deriving stock (Show, Eq)

{- | The two ways a TypeScript file can name what it depends on.

'Relative' is not a third alias: it is a direction from wherever the importing
file sits, so the same text names a different module in every file that writes
it. That is what makes it the interesting case for anything asserting that a
Module Name means one thing.
-}
data Naming
    = Aliased Text
    | Relative
    deriving stock (Show, Eq)

asImports, asReExports, asRelative, indexForm, directoryForm :: Spelling
asImports = Spelling {reExport = False, directory = False, naming = Aliased primaryAlias}
asReExports = asImports {reExport = True}
asRelative = asImports {naming = Relative}
indexForm = asImports
directoryForm = asImports {directory = True}

-- | The alias a canonical name is built from, and one that resolves identically.
primaryAlias, secondaryAlias :: Text
primaryAlias = "@/"
secondaryAlias = "~/"

{- | Modules are drawn from a fixed pool so that generated projects stay small
and readable when a property shrinks one. Half of the pool are barrels.
-}
modulePool :: [Text]
modulePool =
    [ "app/page"
    , "features/home/index"
    , "features/home/service"
    , "lib/util"
    , "lib/index"
    , "server/db"
    ]

genProject :: Gen Project
genProject = do
    count <- Gen.int (Range.linear 1 (length modulePool))
    let names = take count modulePool
    dependencies <- traverse (const . Gen.subsequence $ names) names
    pure . Project $ zipWith ProjectModule names dependencies

-- | Two aliases onto the same directory: the shape issue #117 is about.
projectRoot :: ProjectRoot
projectRoot = ProjectRoot (absPathUnsafe [osp|/home/repo|])

projectConfig :: TsConfig
projectConfig =
    TsConfig
        { pathsBase = absPathUnsafe [osp|/home/repo|]
        , paths =
            [ mkMapping (Wildcard primaryAlias "") [Wildcard "src/" ""]
            , mkMapping (Wildcard secondaryAlias "") [Wildcard "src/" ""]
            ]
        }

projectFiles :: Project -> [OsPath]
projectFiles project = [filePathOf m.name | m <- project.modules]

filePathOf :: Text -> OsPath
filePathOf name = encodeOsPath ("/home/repo/src/" <> name <> ".ts")

renderProject :: Spelling -> Project -> [(AbsPath, Text)]
renderProject spelling project =
    [ (absPathUnsafe . filePathOf $ m.name, renderModule spelling m)
    | m <- project.modules
    ]

renderModule :: Spelling -> ProjectModule -> Text
renderModule spelling m = T.concat [statement d <> "\n" | d <- m.deps]
  where
    statement dep
        | spelling.reExport = "export * from \"" <> specifier dep <> "\";"
        | otherwise = "import { x } from \"" <> specifier dep <> "\";"

    specifier dep = case spelling.naming of
        Aliased alias -> alias <> shorten dep
        Relative -> relativeTo m.name (shorten dep)

    shorten dep
        | spelling.directory, Just shortened <- T.stripSuffix "/index" dep = shortened
        | otherwise = dep

{- | @dep@ spelled from the file that @importer@ names, as a TypeScript author
would write it: @..\/..\/lib\/util@, or @.\/sibling@ for a module alongside.
-}
relativeTo :: Text -> Text -> Text
relativeTo importer dep =
    case (drop (length shared) importerDir, drop (length shared) depSegs) of
        ([], rest) -> T.intercalate "/" ("." : rest)
        (up, rest) -> T.intercalate "/" (replicate (length up) ".." <> rest)
  where
    -- the directory the importing file sits in: its name minus the file part
    importerDir = fromMaybe [] . viaNonEmpty NE.init . T.splitOn "/" $ importer
    depSegs = T.splitOn "/" dep
    shared = map fst . takeWhile (uncurry (==)) $ zip importerDir depSegs

-- | Lowers a rendered project to the modules Deslop reasons about.
projectAsts :: Project -> [(AbsPath, Text)] -> IO [Module]
projectAsts project sources =
    runEff
        . runMockRoFileSystem (mockFiles (projectFiles project))
        . runReader projectConfig
        . runReader projectRoot
        $ traverse (parseModule . program) sources
  where
    program (path, content) = case parseTs TsFile {path = path, content = content} of
        Left err -> TsModule {path = path, cst = error (toText err)}
        Right prog -> prog

{- | What every module can reach, by name. The relation a Rulebook Rule is
judged against, and the thing respelling a project must leave alone.
-}
reachability :: [Module] -> Map Text [Text]
reachability asts =
    runPureEff
        . runReader (buildModuleGraph asts)
        $ Map.fromList <$> traverse reached asts
  where
    reached m = do
        refs <- reachableFrom (ModuleKey m.id)
        pure ((canonicalName m).text, sort [(head r.names).text | r <- refs])
