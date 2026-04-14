{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolver (
    ModuleId (..),
    reverseResolve,
    reverseResolveImport,
    resolve,
    match,
    Match (..),
    isRelativeImport,
) where

import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe, decodeOsPath, encodeOsPath, fsFileExistsAbs, fsMkAbsolute, withAbsBaseSafe)
import System.OsPath (OsPath, dropExtension, takeDirectory)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import Utils (dropCommonPre)

{- | Logical TS module id - e.g. @/lib/util or /src/lib/util (relative to the nearest TS config)
or ./LoginView (relative to the current file) or ../../lib/util (relative to the current file)
or /home/repo/src/lib/util
-}
newtype ModuleId = ModuleId Text deriving stock (Show, Eq)

data ExactPathMapping = ExactPathMapping
    { key :: KeyPattern
    , value :: ValuePattern
    }
    deriving (Show, Eq)

reverseResolveImport ::
    ( RoFileSystem :> es
    , Reader TsConfig :> es
    ) =>
    AbsPath -> ModuleId -> Eff es ModuleId
reverseResolveImport importingFile target = resolve importingFile target >>= reverseResolve

{- | Resolves a TypeScript file's absolute path to a logical 'ModuleId'.

This function performs a "reverse resolution" to determine how a file
should be imported. It processes the path in the following order:
1. Strips the file extension from the absolute path.
2. Calculates the module's path relative to the 'TsConfig' @baseUrl@.
3. Iterates through the TSConfig @paths@ mappings to find an applicable alias.

If a valid path mapping is found, it substitutes the captured path into the
corresponding alias key. If no mappings match, it falls back to the path
relative to the configuration's base URL.

Example:
/home/repo/src/lib/util.tsx -> \@/lib/util (if alias mapped) OR src/lib/util
-}
reverseResolve :: (Reader TsConfig :> es) => AbsPath -> Eff es ModuleId
reverseResolve absFilePath = do
    cfg <- ask @TsConfig
    let noExtAbsFp = dropExtension absFilePath.osPath
    -- src/lib/util
    let (rawRelToCfg, _) = dropCommonPre (decodeOsPath noExtAbsFp, decodeOsPath cfg.baseUrl.osPath)
    let moduleRelToCfg = fromMaybe rawRelToCfg $ T.stripPrefix "/" rawRelToCfg
    pure . ModuleId . fromMaybe moduleRelToCfg $ applyPathMapping cfg.paths moduleRelToCfg
  where
    applyPathMapping :: [PathMapping] -> Text -> Maybe Text
    applyPathMapping [] _ = Nothing
    applyPathMapping (x : xs) moduleRelToCfg
        | Just valueMatch <- matchValues (toList x.values) moduleRelToCfg = case valueMatch of
            ExactMatch -> case x.key of
                (KeyPattern (Exact t)) -> Just t
                (KeyPattern (Wildcard pre suff)) -> Just (pre <> suff)
            WildcardMatch capture -> case x.key of
                -- invalid: Exact Key with Wildcard Value
                (KeyPattern (Exact _)) -> applyPathMapping xs moduleRelToCfg
                (KeyPattern (Wildcard pre suff)) -> Just (pre <> capture <> suff)
        | otherwise = applyPathMapping xs moduleRelToCfg

    matchValues :: [ValuePattern] -> Text -> Maybe Match
    matchValues [] _ = Nothing
    matchValues (ValuePattern p : ps) t
        | Just found <- match p t = Just found
        | otherwise = matchValues ps t

resolve :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> ModuleId -> Eff es AbsPath
resolve importingFile target@(ModuleId mId) =
    if isRelativeImport target
        then
            resolveRelativeImport
        else
            resolveNonRelativeImport
  where
    tsExtensions = [".ts", ".tsx", "/index.ts", "/index.tsx"]

    resolveRelativeImport :: (RoFileSystem :> es) => Eff es AbsPath
    resolveRelativeImport = do
        let importerDir = absPathUnsafe . takeDirectory $ importingFile.osPath
        let targetPath = withAbsBaseSafe importerDir (encodeOsPath mId)

        tryExtensions targetPath tsExtensions
            >>= maybe (fsMkAbsolute targetPath) pure

    resolveNonRelativeImport :: (RoFileSystem :> es, Reader TsConfig :> es) => Eff es AbsPath
    resolveNonRelativeImport = do
        cfg <- ask @TsConfig
        maybePathMapping <- reversePathMapping cfg cfg.paths
        case maybePathMapping of
            Just absPath -> pure absPath
            Nothing -> do
                let fallbackPath = withAbsBaseSafe cfg.baseUrl (encodeOsPath mId)
                tryExtensions fallbackPath tsExtensions
                    >>= maybe (fsMkAbsolute fallbackPath) pure

    reversePathMapping :: (RoFileSystem :> es) => TsConfig -> [PathMapping] -> Eff es (Maybe AbsPath)
    reversePathMapping _ [] = pure Nothing
    reversePathMapping cfg (p : ps)
        | Just keyMatch <- match p.key.pattern mId = do
            maybeAbsPath <- tryValues cfg keyMatch (toList p.values)
            case maybeAbsPath of
                Just absPath -> pure $ Just absPath
                Nothing -> reversePathMapping cfg ps
        | otherwise = reversePathMapping cfg ps

    tryValues :: (RoFileSystem :> es) => TsConfig -> Match -> [ValuePattern] -> Eff es (Maybe AbsPath)
    tryValues _ _ [] = pure Nothing
    tryValues cfg keyMatch ((ValuePattern v) : vs) = do
        let maybeRelToCfg = case (keyMatch, v) of
                (ExactMatch, (Exact t)) -> Just t
                -- invalid: Exact Key with Wildcard Value
                (ExactMatch, (Wildcard _ _)) -> Nothing
                ((WildcardMatch _), (Exact t)) -> Just t
                ((WildcardMatch capture), (Wildcard pre suf)) -> Just (pre <> capture <> suf)
        let cleanRelToCfg = T.dropWhileEnd (== '/') <$> maybeRelToCfg
        let maybeFilePath = (withAbsBaseSafe cfg.baseUrl . encodeOsPath) <$> cleanRelToCfg
        case maybeFilePath of
            Nothing -> tryValues cfg keyMatch vs
            Just filePath ->
                tryExtensions filePath tsExtensions
                    >>= maybe (tryValues cfg keyMatch vs) (pure . Just)

    tryExtensions :: (RoFileSystem :> es) => OsPath -> [Text] -> Eff es (Maybe AbsPath)
    tryExtensions _ [] = pure Nothing
    tryExtensions fp (ext : es) = do
        absFilePath <- fsMkAbsolute (fp <> encodeOsPath ext)
        exists <- fsFileExistsAbs absFilePath
        if exists
            then pure $ Just absFilePath
            else tryExtensions fp es

isRelativeImport :: ModuleId -> Bool
isRelativeImport (ModuleId ".") = True
isRelativeImport (ModuleId "..") = True
isRelativeImport (ModuleId t) =
    "./" `T.isPrefixOf` t
        || "../" `T.isPrefixOf` t
        || "/" `T.isPrefixOf` t

data Match = ExactMatch | WildcardMatch Text deriving (Show, Eq)

match :: Pattern -> Text -> Maybe Match
match (Exact p) t
    | p == t = Just ExactMatch
    | otherwise = Nothing
match (Wildcard pre suff) t
    | T.length t >= T.length pre + T.length suff
    , Just rest <- T.stripPrefix pre t
    , Just capture <- T.stripSuffix suff rest =
        Just (WildcardMatch capture)
    | otherwise = Nothing
