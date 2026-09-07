{- | Turning what a TypeScript file /writes/ in an import into the module it
actually names, and back again.

The 'Deslop.AST.ModuleName' it deals in is the core's, not this module's: what
varies per language is how a written import resolves to a module, not what a
module is.

A file answers to more than one name - the directory and index forms of a
barrel, and every @paths@ alias that maps to it - and 'moduleNames' produces all
of them. Deslop matches patterns against every one, which is what stops the same
file from becoming two vertices.
-}
module TypeScript.ModuleResolver (
    moduleNames,
    reverseResolve,
    reverseResolveImport,
    resolve,
    match,
    Match (..),
    isRelativeImport,
    dropTypeScriptExtension,
) where

import Data.Text qualified as T
import Deslop.AST (ModuleName (..), moduleNameUnsafe)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (RoFileSystem, fsFileExists, fsMkAbsolute)
import FileSystem.Path (
    AbsPath (..),
    absPathUnsafe,
    decodeOsPath,
    dropCommonSegments,
    encodeOsPath,
    withAbsBaseSafe,
 )
import System.OsPath (OsPath, dropExtension, splitDirectories, takeDirectory)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))

{- | Every alias this file answers to, canonical first.

The canonical name is what 'reverseResolve' has always returned, so a Problem
reported against a module keeps the id it had and no Baseline churns. After it
come the remaining aliases, and then the directory form of a barrel - kept only
when the alias that produced it would accept the shortened text, so that
@src\/index.ts@ under @\@\/*@ does not claim the name @\@@.

Empty when no mapping names the file; "TypeScript.AST" supplies the fallback,
because only it knows the project root the fallback is spelled from.
-}
moduleNames :: (Reader TsConfig :> es) => AbsPath -> Eff es [ModuleName]
moduleNames absFilePath = do
    cfg <- ask @TsConfig
    let aliased = aliasNames cfg absFilePath
        directoryForms =
            [ short
            | (key, name) <- aliased
            , Just short <- [T.stripSuffix "/index" name]
            , isJust (match key short)
            ]
    pure . fmap moduleNameUnsafe . ordNub $ fmap snd aliased <> directoryForms

reverseResolveImport ::
    ( RoFileSystem :> es
    , Reader TsConfig :> es
    ) =>
    AbsPath -> ModuleName -> Eff es ModuleName
reverseResolveImport importingFile target = do
    maybeAbsPath <- resolve importingFile target
    case maybeAbsPath of
        Just absPath -> do
            maybeResolved <- reverseResolve absPath
            pure $ case maybeResolved of
                Nothing -> target
                Just resolved ->
                    if resolved.text == target.text <> "/index"
                        then target -- original already names the index implicitly
                        else resolved
        Nothing -> pure target -- keep the original target

-- | The canonical name of a file: the first alias that maps to it, if any.
reverseResolve :: (Reader TsConfig :> es) => AbsPath -> Eff es (Maybe ModuleName)
reverseResolve absFilePath = do
    cfg <- ask @TsConfig
    pure . fmap (moduleNameUnsafe . snd) . listToMaybe $ aliasNames cfg absFilePath

{- | Every @paths@ mapping that names this file, in the config's own precedence
order, paired with the key pattern that produced it. A mapping whose key would
produce a relative name is dropped: a relative name is not a name, it is a
direction from wherever the reader happens to be.
-}
aliasNames :: TsConfig -> AbsPath -> [(Pattern, Text)]
aliasNames cfg absFilePath =
    [ (mapping.key.pattern, name)
    | mapping <- cfg.paths
    , Just name <- [applyPathMapping mapping moduleRelToCfg]
    , not . isRelativeImport . moduleNameUnsafe $ name
    ]
  where
    moduleRelToCfg = T.intercalate "/" (upTraversal <> tRemainder)

    noExtAbsFp = dropTypeScriptExtension absFilePath.osPath
    (tRemainderOsp, bRemainderOsp) =
        dropCommonSegments (splitDirectories noExtAbsFp) (splitDirectories cfg.pathsBase.osPath)
    tRemainder = decodeOsPath <$> tRemainderOsp
    upTraversal = replicate (length bRemainderOsp) ".."

{- | The name one mapping gives a file, if that mapping's value matches it. A
wildcard value under an exact key is not a valid mapping and names nothing.
-}
applyPathMapping :: PathMapping -> Text -> Maybe Text
applyPathMapping mapping moduleRelToCfg = do
    valueMatch <- matchValues (toList mapping.values) moduleRelToCfg
    case (valueMatch, mapping.key) of
        (ExactMatch, KeyPattern (Exact t)) -> Just t
        (ExactMatch, KeyPattern (Wildcard pre suff)) -> Just (pre <> suff)
        (WildcardMatch _, KeyPattern (Exact _)) -> Nothing
        (WildcardMatch capture, KeyPattern (Wildcard pre suff)) -> Just (pre <> capture <> suff)
  where
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
    AbsPath -> ModuleName -> Eff es (Maybe AbsPath)
resolve importingFile target =
    if isRelativeImport target
        then
            Just <$> resolveRelativeImport
        else
            resolveNonRelativeImport
  where
    targetId = target.text
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
        let maybeFilePath = withAbsBaseSafe cfg.pathsBase . encodeOsPath <$> cleanRelToCfg
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

isRelativeImport :: ModuleName -> Bool
isRelativeImport m = case m.text of
    "." -> True
    ".." -> True
    t ->
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
