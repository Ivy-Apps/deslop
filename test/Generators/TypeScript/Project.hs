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
    genProject,
    asImports,
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

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deslop.AST (AstModule (..), ModuleName (..), canonicalName)
import Deslop.CodeGraph (GraphKey (..), ModuleRef (..), buildModuleGraph, reachableFrom)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff, runPureEff)
import Effectful.Reader.Static (runReader)
import Fixtures.TypeScript.Config (mkMapping)
import FileSystem.Path (AbsPath, ProjectRoot (..), absPathUnsafe, encodeOsPath)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (OsPath, osp)
import TypeScript.AST (parseAst)
import TypeScript.Config (Pattern (..), TsConfig (..))
import TypeScript.CST (TsProgram (..))
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
    , alias :: Text
    -- ^ Which of the two aliases mapping to @src\/@ to write.
    }
    deriving stock (Show, Eq)

asImports, asReExports, indexForm, directoryForm :: Spelling
asImports = Spelling {reExport = False, directory = False, alias = primaryAlias}
asReExports = asImports {reExport = True}
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

    specifier dep = spelling.alias <> shorten dep

    shorten dep
        | spelling.directory, Just shortened <- T.stripSuffix "/index" dep = shortened
        | otherwise = dep

-- | Lowers a rendered project to the modules Deslop reasons about.
projectAsts :: Project -> [(AbsPath, Text)] -> IO [AstModule]
projectAsts project sources =
    runEff
        . runMockRoFileSystem (mockFiles (projectFiles project))
        . runReader projectConfig
        . runReader projectRoot
        $ traverse (parseAst . program) sources
  where
    program (path, content) = case parseTs TsFile {path = path, content = content} of
        Left err -> TsModule {path = path, cst = error (toText err)}
        Right prog -> prog

{- | What every module can reach, by name. The relation a Rulebook Rule is
judged against, and the thing respelling a project must leave alone.
-}
reachability :: [AstModule] -> Map Text [Text]
reachability asts =
    runPureEff
        . runReader (buildModuleGraph asts)
        $ Map.fromList <$> traverse reached asts
  where
    reached m = do
        refs <- reachableFrom (InternalKey m.id)
        pure ((canonicalName m).text, sort [(head r.names).text | r <- refs])
