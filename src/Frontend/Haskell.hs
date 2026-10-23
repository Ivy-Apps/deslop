{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE: Haskell as a 'CompilerFrontend'. Throwaway, to probe PR #237.

A project with a @cabal.project@ or a @*.cabal@ file at its root. Each package's
@hs-source-dirs@ are walked for @.hs@ files. The "parser" only recognises
@module@ and @import@ lines that start in column 0 and keeps every line's raw
text, so 'render' is the identity. Lowering builds a module index over every
unit and resolves each import against it; anything else is 'External'.
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

newtype HsParseError = HsParseError AbsPath
    deriving stock (Show, Eq)

-- The driver logs only this text, so the path has to be in it.
instance Renderable HsParseError where
    render (HsParseError p) = decodeOsPath p.osPath <> ": not valid UTF-8"

-- | One cabal package: where it lives and where its sources are.
newtype HsPackage = HsPackage {sourceDirs :: [AbsPath]}

newtype HsConfig = HsConfig {packages :: [HsPackage]}

data Decl
    = ModuleHeader Text
    | Import Text
    | Other

data HsLine = HsLine Text Decl

rawOf :: HsLine -> Text
rawOf (HsLine raw _) = raw

data HsFile = HsFile {path :: AbsPath, hsLines :: [HsLine]}

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
    discover fe cfg _ gitIgnore =
        sortNub . concat <$> traverse (walkDir skip hsSource) (concatMap (.sourceDirs) cfg.packages)
      where
        -- The driver already handed 'ecosystemIgnored' to 'loadGitIgnore', yet
        -- 'isIgnored' does not apply it, so every frontend has to OR it in.
        skip entry = ecosystemIgnored fe entry || isIgnored gitIgnore entry
        hsSource entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.hs|])
    -- No Config here, so default-extensions (CPP!) from the .cabal file cannot
    -- change how a file is read.
    parse _ path bytes = case decodeUtf8' bytes of
        Left _ -> Left (HsParseError path)
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
    lower _ _ files = do
        projectRoot <- ask @ProjectRoot
        let named = (\f -> (f, nameOf f)) <$> files
            -- Two executables both define `Main`: the first one wins the name,
            -- and both mint the same ModuleName.
            index :: Map Text ModuleId
            index = Map.fromListWith (\_ first' -> first') [(n, idOf f) | (f, n) <- named]
        pure $ moduleOf projectRoot index <$> named
      where
        idOf f = moduleIdUnsafe (decodeOsPath f.path.osPath)
        moduleOf projectRoot index (f, name) =
            Module
                { id = idOf f
                , names = slashed name :| []
                , path = relativePathTo projectRoot f.path
                , edges = mapMaybe (edgeOf projectRoot index f) (zip [1 ..] f.hsLines)
                }
        edgeOf projectRoot index f (line, HsLine raw decl) = case decl of
            Import name ->
                Just
                    DependencyEdge
                        { specifier = specifierUnsafe name
                        , target = maybe (External (specifierUnsafe name)) (\mid -> Resolved mid (slashed name :| [])) (Map.lookup name index)
                        , kind = ImportEdge
                        , location = Location {file = relativePathTo projectRoot f.path, line, code = T.strip raw}
                        }
            _ -> Nothing

slashed :: Text -> ModuleName
slashed = moduleNameUnsafe . T.replace "." "/"

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
expectedName cfg file = viaNonEmpty head . sortOn (Down . T.length) $ mapMaybe from dirs
  where
    dirs = concatMap (.sourceDirs) cfg.packages
    from dir = do
        rest <- T.stripPrefix (decodeOsPath dir.osPath <> "/") (decodeOsPath file.osPath)
        T.replace "/" "." <$> T.stripSuffix ".hs" rest

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
            (cabal : _) -> do
                fields <- fieldValues "hs-source-dirs" . decodeUtf8 <$> fsReadFile cabal
                let srcs = bool fields ["."] (null fields)
                pure . Just . HsPackage $ withAbsBaseUnsafe dir . encodeOsPath <$> srcs
            [] -> do
                cliLog Warning $ "WARNING: no .cabal file in " <> decodeOsPath dir.osPath
                pure Nothing

cabalFilesIn :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
cabalFilesIn dir = filter ((== [osp|.cabal|]) . takeExtension . (.osPath)) <$> fsListDirectory dir

{- | Every value of a field, wherever it appears, including indented
continuation lines. Crude: no sections, no conditionals, no comments.
-}
fieldValues :: Text -> Text -> [Text]
fieldValues field = go . lines
  where
    go [] = []
    go (l : ls) = case T.stripPrefix (field <> ":") (T.toLower . T.strip $ l) of
        Just _ ->
            let rest = T.drop 1 . T.dropWhile (/= ':') $ l
                (continued, others) = span (\c -> indent c > indent l && not (":" `T.isInfixOf` c)) ls
             in values (rest : continued) <> go others
        Nothing -> go ls
    indent = T.length . T.takeWhile isSpace
    values = filter (not . T.null) . concatMap (T.split (\c -> c == ',' || isSpace c))
