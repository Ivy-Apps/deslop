module TypeScript.ModuleResolver (
    ModuleId (text),
    moduleIdUnsafe,
    reverseResolve,
    reverseResolveImport,
    resolve,
    match,
    Match (..),
    isRelativeImport,
    dropTypeScriptExtension,
) where

import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe, decodeOsPath, encodeOsPath, fsFileExists, fsMkAbsolute, withAbsBaseSafe)
import System.OsPath (OsPath, dropExtension, splitDirectories, takeDirectory)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))

{- | Logical TS module id - e.g. @/lib/util or /src/lib/util (relative to the nearest TS config)
or ./LoginView (relative to the current file) or ../../lib/util (relative to the current file)
or /home/repo/src/lib/util
-}
newtype ModuleId = ModuleId
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)

moduleIdUnsafe :: Text -> ModuleId
moduleIdUnsafe = ModuleId

reverseResolveImport ::
    ( RoFileSystem :> es
    , Reader TsConfig :> es
    ) =>
    AbsPath -> ModuleId -> Eff es ModuleId
reverseResolveImport importingFile target = do
    maybeAbsPath <- resolve importingFile target
    case maybeAbsPath of
        Just absPath ->
            reverseResolve absPath
                >>= pure . fromMaybe target
        Nothing -> pure target -- keep the original target

reverseResolve :: (Reader TsConfig :> es) => AbsPath -> Eff es (Maybe ModuleId)
reverseResolve absFilePath = do
    cfg <- ask @TsConfig
    let noExtAbsFp = dropTypeScriptExtension absFilePath.osPath
    let targetSegs = splitDirectories noExtAbsFp
    let baseUrlSegs = splitDirectories cfg.baseUrl.osPath
    let (tRemainderOsp, bRemainderOsp) = dropCommonSegments targetSegs baseUrlSegs
    let tRemainder = decodeOsPath <$> tRemainderOsp

    let upTraversal = replicate (length bRemainderOsp) ".."
    let moduleRelToCfg = T.intercalate "/" (upTraversal <> tRemainder)
    case applyPathMapping cfg.paths moduleRelToCfg of
        Just alias ->
            let moduleId = ModuleId alias
             in if isRelativeImport moduleId
                    then pure Nothing
                    else pure $ Just moduleId
        Nothing -> pure Nothing
  where
    dropCommonSegments :: (Eq a) => [a] -> [a] -> ([a], [a])
    dropCommonSegments (x : xs) (y : ys) | x == y = dropCommonSegments xs ys
    dropCommonSegments xs ys = (xs, ys)

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

dropTypeScriptExtension :: OsPath -> OsPath
dropTypeScriptExtension osp
    | any (`T.isSuffixOf` path) [".d.ts", ".d.mts", ".d.cts"] = dropExtension (dropExtension osp)
    | any
        (`T.isSuffixOf` path)
        [ ".ts"
        , ".tsx"
        , ".mts"
        , ".cts"
        , ".js"
        , ".jsx"
        , ".mjs"
        , ".cjs"
        ] =
        dropExtension osp
    | otherwise = osp
  where
    path = decodeOsPath osp

resolve ::
    ( RoFileSystem :> es
    , Reader TsConfig :> es
    ) =>
    AbsPath -> ModuleId -> Eff es (Maybe AbsPath)
resolve importingFile target@(ModuleId targetId) =
    if isRelativeImport target
        then
            Just <$> resolveRelativeImport
        else
            resolveNonRelativeImport
  where
    tsExtensions = [".ts", ".tsx", "/index.ts", "/index.tsx"]

    resolveRelativeImport :: (RoFileSystem :> es) => Eff es AbsPath
    resolveRelativeImport = do
        let importerDir = absPathUnsafe . takeDirectory $ importingFile.osPath
        let targetPath = withAbsBaseSafe importerDir (encodeOsPath targetId)

        tryExtensions targetPath tsExtensions
            >>= maybe (fsMkAbsolute targetPath) pure

    resolveNonRelativeImport :: (RoFileSystem :> es, Reader TsConfig :> es) => Eff es (Maybe AbsPath)
    resolveNonRelativeImport = do
        cfg <- ask @TsConfig
        reversePathMapping cfg cfg.paths

    reversePathMapping :: (RoFileSystem :> es) => TsConfig -> [PathMapping] -> Eff es (Maybe AbsPath)
    reversePathMapping _ [] = pure Nothing
    reversePathMapping cfg (p : ps)
        | Just keyMatch <- match p.key.pattern targetId = do
            maybeAbsPath <- tryValues cfg keyMatch (toList p.values)
            case maybeAbsPath of
                Just absPath -> pure $ Just absPath
                Nothing -> reversePathMapping cfg ps
        | otherwise = reversePathMapping cfg ps

    tryValues :: (RoFileSystem :> es) => TsConfig -> Match -> [ValuePattern] -> Eff es (Maybe AbsPath)
    tryValues _ _ [] = pure Nothing
    tryValues cfg keyMatch ((ValuePattern v) : vs) = do
        let maybeRelToCfg = case (keyMatch, v) of
                (ExactMatch, Exact t) -> Just t
                -- invalid: Exact Key with Wildcard Value
                (ExactMatch, Wildcard _ _) -> Nothing
                (WildcardMatch _, Exact t) -> Just t
                (WildcardMatch capture, Wildcard pre suf) -> Just (pre <> capture <> suf)
        let cleanRelToCfg = T.dropWhileEnd (== '/') <$> maybeRelToCfg
        let maybeFilePath = withAbsBaseSafe cfg.baseUrl . encodeOsPath <$> cleanRelToCfg
        case maybeFilePath of
            Nothing -> tryValues cfg keyMatch vs
            Just filePath ->
                tryExtensions filePath tsExtensions
                    >>= maybe (tryValues cfg keyMatch vs) (pure . Just)

    tryExtensions :: (RoFileSystem :> es) => OsPath -> [Text] -> Eff es (Maybe AbsPath)
    tryExtensions _ [] = pure Nothing
    tryExtensions fp (ext : es) = do
        absFilePath <- fsMkAbsolute (fp <> encodeOsPath ext)
        exists <- fsFileExists absFilePath
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
