{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE: Haskell as a 'CompilerFrontend'. Throwaway, to probe PR #237.

A project with a @cabal.project@ or a @*.cabal@ file at its root. Each package's
components (library, executables, test suites, benchmarks) are read from its
@.cabal@ file, and their @hs-source-dirs@ are walked for @.hs@ files. The
"parser" only recognises @module@ and @import@ lines that start in column 0 and
keeps every line's raw text, so 'render' is the identity.

A Module is a file /as compiled by one component/. Its canonical Module Name is
qualified by that component - @lib\/hs-tiny\/Tiny\/A@, @exe\/hs-tiny-tool\/Main@ -
and it also answers to the bare GHC name (@Tiny\/A@, @Main@). The canonical name
is what Problem ids are built from, so two executables' @Main@s never share an
id, and a file two components both compile is two Modules that each resolve
their imports against their own component's index.
-}
module Frontend.Haskell (
    HsFrontend (..),
    HsParseError (..),
) where

import Data.Char (isAlphaNum, isSpace)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (unfixable)
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleId,
    ModuleName,
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import Deslop.Problem (LintRuleId (..), Problem (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (ask)
import Effects.CLI (CLI, LogStyle (..), cliLog)
import Effects.FileSystem (RoFileSystem, fsFileExists, fsListDirectory, fsReadFile)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (
    AbsPath (..),
    ProjectRoot (..),
    decodeOsPath,
    encodeOsPath,
    relativePathTo,
    withAbsBaseUnsafe,
 )
import Git.Ignore (isIgnored)
import Renderable (Renderable)
import Renderable qualified
import System.OsPath (osp, takeDirectory, takeExtension, takeFileName)

data HsFrontend = HsFrontend
    deriving stock (Show, Eq)

-- | The driver names the file, so this says only what is wrong with it.
data HsParseError = NotUtf8 | UsesCpp
    deriving stock (Show, Eq)

instance Renderable HsParseError where
    render = \case
        NotUtf8 -> "not valid UTF-8"
        UsesCpp -> "compiled with CPP (default-extensions), which this frontend cannot read yet"

data ComponentKind = Lib | Exe | Test | Bench
    deriving stock (Eq)

kindPrefix :: ComponentKind -> Text
kindPrefix = \case
    Lib -> "lib"
    Exe -> "exe"
    Test -> "test"
    Bench -> "bench"

{- | One cabal component. A library compiles every file under its source dirs;
anything with a @main@ compiles only @main-is@ plus @other-modules@, because two
executables may share a source dir and must not claim each other's files.
-}
data Component = Component
    { kind :: ComponentKind
    , name :: Text
    -- ^ The main library is named after its package, as cabal does.
    , sourceDirs :: [AbsPath]
    , mainIs :: Maybe Text
    , otherModules :: [Text]
    , depends :: [Text]
    -- ^ Package names only; version bounds dropped.
    , extensions :: [Text]
    -- ^ @default-extensions@, for 'parse' once it is handed the Config.
    }

-- | What every Module Name of this component starts with: @exe/hs-tiny-tool@.
qualifier :: Component -> Text
qualifier c = kindPrefix c.kind <> "/" <> c.name

data HsPackage = HsPackage {name :: Text, components :: [Component]}

newtype HsConfig = HsConfig {packages :: [HsPackage]}

allComponents :: HsConfig -> [Component]
allComponents = concatMap (.components) . (.packages)

data Decl
    = ModuleHeader Text
    | Import Text
    | Other

data HsLine = HsLine Text Decl

rawOf :: HsLine -> Text
rawOf (HsLine raw _) = raw

data HsFile = HsFile {path :: AbsPath, hsLines :: [HsLine]}

-- | The @default-extensions@ of every component whose source dirs hold the file.
extensionsFor :: HsConfig -> AbsPath -> [Text]
extensionsFor cfg file =
    sortNub [e | c <- allComponents cfg, any (isJust . (`relativeName` file)) c.sourceDirs, e <- c.extensions]

moduleNameRule :: LintRuleId
moduleNameRule = LintRuleId "haskell-module-name-matches-path"

instance CompilerFrontend HsFrontend where
    type Config HsFrontend = HsConfig
    type Unit HsFrontend = HsFile
    type ParseError HsFrontend = HsParseError

    detect projectRoot = do
        project <- fsFileExists (withAbsBaseUnsafe projectRoot.path [osp|cabal.project|])
        cabals <- cabalFilesIn projectRoot.path
        pure . bool Nothing (Just HsFrontend) $ project || not (null cabals)
    label _ = "Haskell"
    lookingFor _ = "Haskell (cabal.project or *.cabal)"
    codeFence _ = CodeFence "haskell"
    builtInRules _ = [moduleNameRule]
    ecosystemIgnored _ = (`elem` ignoredNames) . takeFileName . (.osPath) . (.path)
      where
        ignoredNames = encodeOsPath <$> ["dist-newstyle", "dist", ".stack-work", ".hie"]
    loadConfig _ = loadHsConfig
    discover _ cfg _ gitIgnore =
        sortNub . concat <$> traverse (walkDir (isIgnored gitIgnore) hsSource) (sortNub . concatMap (.sourceDirs) . allComponents $ cfg)
      where
        hsSource entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.hs|])
    -- The file is read once, but every component compiling it may enable
    -- different default-extensions; the union is the best parse can do. The
    -- line classifier cannot see through CPP, so it refuses rather than misread.
    parse _ cfg path bytes
        | "CPP" `elem` extensionsFor cfg path = Left UsesCpp
        | otherwise = case decodeUtf8' bytes of
            Left _ -> Left NotUtf8
            Right text -> Right . HsFile path . fmap classify . T.splitOn "\n" $ text
    render _ = TE.encodeUtf8 . T.intercalate "\n" . fmap rawOf . (.hsLines)
    lint _ cfg file = do
        projectRoot <- ask @ProjectRoot
        pure . maybeToList $ do
            expected <- expectedName cfg file.path
            (line, actual, raw) <- header file
            guard (actual /= expected)
            -- Unfixable: renaming the header is a one-line Endo, but every
            -- importer and the .cabal module list would also need rewriting,
            -- and a Fix can only edit this one unit.
            pure . unfixable $ \autoFixable ->
                LintProblem
                    { lintRule = moduleNameRule
                    , location = Location {file = relativePathTo projectRoot file.path, line, code = raw}
                    , description = "Module `" <> actual <> "` lives in a file that GHC would look up as `" <> expected <> "`."
                    , fix = "Rename the module to `" <> expected <> "`, or move the file."
                    , autoFixable
                    }
    lower _ cfg files = do
        projectRoot <- ask @ProjectRoot
        pure $ concatMap (lowerComponent projectRoot cfg files) (allComponents cfg)

-- | Every file a component compiles, lowered as that component sees it.
lowerComponent :: ProjectRoot -> HsConfig -> [HsFile] -> Component -> [Module]
lowerComponent projectRoot cfg files component = moduleOf <$> own
  where
    own = ownedBy component files
    -- What this component can import: its own modules, then the libraries of
    -- the project packages it depends on. A name both define resolves to its own.
    index :: Map Text (ModuleId, NonEmpty ModuleName)
    index = Map.fromList . reverse $ visible
    visible =
        [(nameOf f, targetOf component f) | f <- own]
            <> [ (nameOf f, targetOf lib f)
               | pkg <- cfg.packages
               , pkg.name `elem` component.depends
               , lib <- filter ((== Lib) . (.kind)) pkg.components
               , f <- ownedBy lib files
               ]
    targetOf c f = (idOf c f, namesIn c f)
    moduleOf f =
        Module
            { id = idOf component f
            , names = namesIn component f
            , path = relativePathTo projectRoot f.path
            , edges = mapMaybe (edgeOf f) (zip [1 ..] f.hsLines)
            }
    edgeOf f (line, HsLine raw decl) = case decl of
        Import name ->
            Just
                DependencyEdge
                    { specifier = specifierUnsafe name
                    , -- The core turns an External's specifier into its Module
                      -- Name, so it is slashed here (ADR 19); the edge's own
                      -- specifier above keeps what the source typed.
                      target = maybe (External . specifierUnsafe . slashed $ name) (uncurry Resolved) (Map.lookup name index)
                    , kind = ImportEdge
                    , location = Location {file = relativePathTo projectRoot f.path, line, code = T.strip raw}
                    }
        _ -> Nothing

{- | Canonical first: the component-qualified name, which Problem ids are built
from, then the bare GHC name, so @Tiny/**@ and @Main@ still match.
-}
namesIn :: Component -> HsFile -> NonEmpty ModuleName
namesIn c f = moduleNameUnsafe (qualifier c <> "/" <> bare) :| [moduleNameUnsafe bare]
  where
    bare = slashed (nameOf f)

-- | One file compiled by two components is two Modules, so the id names both.
idOf :: Component -> HsFile -> ModuleId
idOf c f = moduleIdUnsafe (qualifier c <> "|" <> decodeOsPath f.path.osPath)

ownedBy :: Component -> [HsFile] -> [HsFile]
ownedBy c = filter owns
  where
    owns f = case c.mainIs of
        Nothing -> any (isJust . (`relativeName` f.path)) c.sourceDirs
        Just mainFile ->
            any (\dir -> decodeOsPath f.path.osPath == decodeOsPath dir.osPath <> "/" <> mainFile) c.sourceDirs
                || any (maybe False (`elem` c.otherModules) . (`relativeName` f.path)) c.sourceDirs

-- | The module name GHC would look a file up by, from one source dir.
relativeName :: AbsPath -> AbsPath -> Maybe Text
relativeName dir file = do
    rest <- T.stripPrefix (decodeOsPath dir.osPath <> "/") (decodeOsPath file.osPath)
    T.replace "/" "." <$> T.stripSuffix ".hs" rest

slashed :: Text -> Text
slashed = T.replace "." "/"

nameOf :: HsFile -> Text
nameOf = maybe "Main" (\(_, n, _) -> n) . header

header :: HsFile -> Maybe (Int, Text, Text)
header file = listToMaybe [(i, n, raw) | (i, HsLine raw (ModuleHeader n)) <- zip [1 ..] file.hsLines]

classify :: Text -> HsLine
classify raw = HsLine raw $ case T.words raw of
    ("module" : rest) -> maybe Other ModuleHeader (nameIn rest)
    ("import" : rest) -> maybe Other Import (nameIn . filter (not . noise) $ rest)
    _ -> Other
  where
    noise w = w `elem` ["safe", "qualified", "{-#", "SOURCE", "#-}", "{-#SOURCE#-}"] || "\"" `T.isPrefixOf` w
    nameIn = \case
        (w : _) | let n = T.takeWhile isNameChar w, not (T.null n) -> Just n
        _ -> Nothing
    isNameChar c = isAlphaNum c || c `elem` ['.', '_', '\'']

-- | What GHC would call the file, from the source dir that contains it.
expectedName :: HsConfig -> AbsPath -> Maybe Text
expectedName cfg file = viaNonEmpty head . sortOn (Down . T.length) $ mapMaybe (`relativeName` file) dirs
  where
    dirs = concatMap (.sourceDirs) (allComponents cfg)

loadHsConfig :: (RoFileSystem :> es, CLI :> es) => ProjectRoot -> Eff es HsConfig
loadHsConfig projectRoot = do
    let projectFile = withAbsBaseUnsafe projectRoot.path [osp|cabal.project|]
    hasProject <- fsFileExists projectFile
    dirs <-
        if hasProject
            then fmap packageDir . fieldValues "packages" . decodeUtf8 <$> fsReadFile projectFile
            else pure [projectRoot.path]
    HsConfig . catMaybes <$> traverse package dirs
  where
    packageDir p
        | ".cabal" `T.isSuffixOf` p = withAbsBaseUnsafe projectRoot.path (takeDirectory . encodeOsPath $ p)
        | otherwise = withAbsBaseUnsafe projectRoot.path (encodeOsPath . T.dropWhileEnd (== '/') $ p)
    package dir =
        cabalFilesIn dir >>= \case
            (cabal : _) -> Just . packageOf dir . decodeUtf8 <$> fsReadFile cabal
            [] -> do
                cliLog Warning $ "WARNING: no .cabal file in " <> decodeOsPath dir.osPath
                pure Nothing

{- | A package's components, from the text of its @.cabal@ file. Crude: no
conditionals, no @common@ stanzas, no sub-library dependencies.
-}
packageOf :: AbsPath -> Text -> HsPackage
packageOf dir cabal = HsPackage {name = pkgName, components = mapMaybe component stanzas}
  where
    (topLevel, stanzas) = stanzasOf cabal
    pkgName = fromMaybe "" . listToMaybe $ fieldValues "name" topLevel
    component (stanzaHeader, body) = do
        (kind, name) <- case T.words (T.toLower stanzaHeader) of
            ["library"] -> Just (Lib, pkgName)
            ["library", n] -> Just (Lib, n)
            ["executable", n] -> Just (Exe, n)
            ["test-suite", n] -> Just (Test, n)
            ["benchmark", n] -> Just (Bench, n)
            _ -> Nothing
        let srcs = fieldValues "hs-source-dirs" body
        pure
            Component
                { kind
                , name
                , sourceDirs = withAbsBaseUnsafe dir . encodeOsPath <$> bool srcs ["."] (null srcs)
                , mainIs = bool Nothing (listToMaybe $ fieldValues "main-is" body) (kind /= Lib)
                , otherModules = fieldValues "other-modules" body
                , depends = mapMaybe packageName . T.splitOn "," . unwords . fieldLines "build-depends" $ body
                , extensions = fieldValues "default-extensions" body
                }
    packageName = listToMaybe . T.words . T.takeWhile (`notElem` ['<', '>', '=', '^'])

{- | Top-level lines, then each stanza's header and body. A stanza starts at an
unindented line that is not a field, and runs until the next one.
-}
stanzasOf :: Text -> (Text, [(Text, Text)])
stanzasOf text = (unlines top, groups rest)
  where
    (top, rest) = break isHeader (lines text)
    isHeader l = not (T.null l) && not (isSpace (T.head l)) && not (":" `T.isInfixOf` l) && not ("--" `T.isPrefixOf` l)
    groups = \case
        [] -> []
        (h : ls) -> let (body, others) = break isHeader ls in (h, unlines body) : groups others

cabalFilesIn :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
cabalFilesIn dir = filter ((== [osp|.cabal|]) . takeExtension . (.osPath)) <$> fsListDirectory dir

{- | Every value of a field, wherever it appears, including indented
continuation lines. Crude: no sections, no conditionals, no comments.
-}
fieldValues :: Text -> Text -> [Text]
fieldValues field = concatMap (filter (not . T.null) . T.split (\c -> c == ',' || isSpace c)) . fieldLines field

-- | The raw text of every occurrence of a field, continuation lines included.
fieldLines :: Text -> Text -> [Text]
fieldLines field = go . lines
  where
    go [] = []
    go (l : ls) = case T.stripPrefix (field <> ":") (T.toLower . T.strip $ l) of
        Just _ ->
            let rest = T.drop 1 . T.dropWhile (/= ':') $ l
                (continued, others) = span (\c -> indent c > indent l && not (":" `T.isInfixOf` c)) ls
             in (rest : continued) <> go others
        Nothing -> go ls
    indent = T.length . T.takeWhile isSpace
