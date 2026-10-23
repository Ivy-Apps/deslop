{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE: Kotlin as a 'CompilerFrontend'. A Gradle build with a
@settings.gradle(.kts)@ at its root, whose @.kt@ files lower to one
'Deslop.Module.Module' per package.

Throwaway exploration of the CompilerFrontend abstraction; see the spike PR.
-}
module Frontend.Kotlin (
    KtFrontend (..),
    KtConfig (..),
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Error (DeslopError (..))
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (Finding, fixable, unfixable)
import Deslop.Module (Location (..))
import Deslop.Problem (LintRuleId (..), Problem (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful (Eff, type (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask)
import Effects.CLI (CLI, LogStyle (..), cliLog)
import Effects.FileSystem (RoFileSystem, fsDirectoryExists, fsFileExists, fsReadFile)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (..), ProjectRelativePath (..), ProjectRoot (..), decodeOsPath, encodeOsPath, relativePathTo, withAbsBaseUnsafe)
import Frontend.Kotlin.Module (lowerKt)
import Frontend.Kotlin.Source
import Git.Ignore (GitIgnore, alwaysIgnored, isIgnored)
import System.OsPath (OsPath, osp, splitDirectories, takeDirectory, takeExtension, takeFileName)

data KtFrontend = KtFrontend
    deriving stock (Show, Eq)

-- | The Gradle modules @settings.gradle.kts@ includes, by directory.
newtype KtConfig = KtConfig
    { gradleModules :: [(Text, AbsPath)]
    }
    deriving stock (Show, Eq)

instance CompilerFrontend KtFrontend where
    type Config KtFrontend = KtConfig
    type Unit KtFrontend = KtFile
    type ParseError KtFrontend = KtParseError

    detect root = do
        found <- traverse (fsFileExists . withAbsBaseUnsafe root.path) settingsFiles
        pure $ KtFrontend <$ guard (or found)
    label _ = "Kotlin"
    lookingFor _ = "Kotlin (settings.gradle.kts)"
    codeFence _ = CodeFence "kotlin"
    builtInRules _ = [packageMatchesDirectoryRule, redundantSamePackageImportRule]
    ecosystemIgnored _ = gradleIgnored
    loadConfig _ = loadSettings
    discover _ cfg _ gitIgnore = concat <$> traverse (ktFiles gitIgnore dirs) dirs
      where
        dirs = snd <$> cfg.gradleModules
    parse _ = parseKt
    render _ = renderKt
    lint _ _ = lintKt
    lower _ _ = lowerKt

settingsFiles :: [OsPath]
settingsFiles = [[osp|settings.gradle.kts|], [osp|settings.gradle|]]

-- | Gradle and IDE output. @build@ also hides KSP/kapt generated sources.
gradleIgnored :: Entry -> Bool
gradleIgnored = (`Set.member` names) . takeFileName . (.osPath) . (.path)
  where
    names = Set.fromList . fmap encodeOsPath $ ["build", ".gradle", ".idea", ".kotlin", "out"]

{- | Reads @include(":a", ":b:c")@ out of the settings script by regex-free
scraping. A real frontend would have to evaluate Kotlin script or ask Gradle.
-}
loadSettings ::
    (RoFileSystem :> es, CLI :> es, Error DeslopError :> es) =>
    ProjectRoot -> Eff es KtConfig
loadSettings root = do
    present <- filterM fsFileExists . fmap (withAbsBaseUnsafe root.path) $ settingsFiles
    case present of
        [] -> throwError . FrontendError $ "no settings.gradle(.kts)"
        settings : _ -> do
            txt <- decodeUtf8 <$> fsReadFile settings
            let included = concatMap includes . T.lines $ txt
            modules <- filterM (fsDirectoryExists . snd) . fmap (\m -> (m, dirOf m)) $ included
            when (null modules) . cliLog Warning $ "WARNING: no Gradle module included; scanning the root only"
            pure . KtConfig $ (":", root.path) : modules
  where
    includes line = case T.stripPrefix "include(" (T.strip line) of
        Just rest -> fmap unquote . T.splitOn "," . T.takeWhile (/= ')') $ rest
        Nothing -> []
    unquote = T.filter (\c -> c /= '"' && c /= '\'') . T.strip
    dirOf m = withAbsBaseUnsafe root.path . encodeOsPath . T.intercalate "/" . filter (not . T.null) . T.splitOn ":" $ m

{- | @.kt@ files under one Gradle module, not descending into a nested Gradle
module (that one is walked on its own) - the root module would otherwise walk
every other.
-}
ktFiles :: (RoFileSystem :> es) => GitIgnore -> [AbsPath] -> AbsPath -> Eff es [AbsPath]
ktFiles gitIgnore moduleDirs moduleDir = walkDir prune select moduleDir
  where
    -- The same three-way prune every frontend's discover has to spell out.
    prune e = alwaysIgnored e || gradleIgnored e || isIgnored gitIgnore e || nestedModule e
    nestedModule e = e.isDir && e.path /= moduleDir && e.path `elem` moduleDirs
    select e = e.path <$ guard (not e.isDir && takeExtension e.path.osPath == [osp|.kt|])

packageMatchesDirectoryRule :: LintRuleId
packageMatchesDirectoryRule = LintRuleId "kotlin-package-matches-directory"

redundantSamePackageImportRule :: LintRuleId
redundantSamePackageImportRule = LintRuleId "kotlin-no-same-package-import"

lintKt :: (Reader ProjectRoot :> es) => KtFile -> Eff es [Finding KtFile]
lintKt file = do
    root <- ask @ProjectRoot
    let rel = relativePathTo root file.path
    pure $ packageVsDirectory rel <> samePackageImports rel
  where
    pkg = packageOf file

    -- Only judged under a conventional source root (src/<set>/kotlin/...).
    packageVsDirectory rel = case sourceRelativeDirs rel of
        Just dirs
            | dirs /= pkg ->
                [ unfixable $ \auto ->
                    LintProblem
                        { lintRule = packageMatchesDirectoryRule
                        , location = headerLocation rel
                        , description =
                            "Package `" <> T.intercalate "." pkg <> "` lives in directory `" <> T.intercalate "/" dirs <> "`."
                        , fix = "Move the file to the directory its package names. (A move is not an `Endo KtFile`, so `deslop fix` cannot.)"
                        , autoFixable = auto
                        }
                ]
        _ -> []

    samePackageImports rel =
        [ fixable
            ( \auto ->
                LintProblem
                    { lintRule = redundantSamePackageImportRule
                    , location = Location {file = rel, line = n, code = code}
                    , description = "`" <> T.strip code <> "` imports from the file's own package, which needs no import."
                    , fix = "Delete the import."
                    , autoFixable = auto
                    }
            )
            (Endo $ dropLine code)
        | (n, imp, code) <- importsOf file
        , not imp.wildcard
        , take (length pkg) imp.path == pkg
        , length imp.path == length pkg + 1
        ]

    dropLine code f = f {body = filter (not . isImport code) f.body}
    isImport code = \case
        ImportLine _ t -> t == code
        _ -> False

    headerLocation rel =
        Location {file = rel, line = 1, code = fromMaybe "" . viaNonEmpty head $ [t | PackageLine _ t <- file.body]}

-- | @core/src/commonMain/kotlin/a/b/X.kt@ is in @a/b@ of source set @commonMain@.
sourceRelativeDirs :: ProjectRelativePath -> Maybe [Text]
sourceRelativeDirs rel =
    case break (== "src") . fmap decodeOsPath . splitDirectories . takeDirectory $ rel.osPath of
        (_, "src" : _sourceSet : lang : rest) | lang `elem` ["kotlin", "java"] -> Just rest
        _ -> Nothing
