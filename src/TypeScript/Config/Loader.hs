{- | Reading a project's TypeScript configuration off disk: the only part of
config handling that touches IO.

A config is rarely one file. @extends@ makes it a chain, and this module walks
it - resolving each link, decoding each file, and folding what every file
declares into one "TypeScript.Config".

The chain is walked depth-first and bases are emitted before the config that
named them, so folding the result left to right lets a config outrank
everything it extends. For an array @extends@ the entries are walked in order,
which gives the compiler's rule that a later entry wins over an earlier one.

Cycles are detected against the /in-progress/ chain rather than a set of every
file seen, because two branches extending one shared base is a diamond, not a
cycle, and is perfectly legal.

'TsConfigLoadError' deliberately stays here rather than in "Deslop.Error": it
names files, which only this module knows about, and the run's error is the
rendered message.
-}
module TypeScript.Config.Loader (
    loadTsConfig,
    TsConfigLoadError (..),
    SkippedExtends (..),
    renderTsConfigLoadError,
    renderSkippedExtends,
) where

import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effects.FileSystem (RoFileSystem, fsFileExists, fsMkAbsolute, fsReadFile)
import FileSystem.Path (
    AbsPath (..),
    ProjectRoot,
    RelativePath (..),
    absPathUnsafe,
    decodeOsPath,
    encodeOsPath,
    relativePathTo,
    withAbsBaseSafe,
 )
import System.OsPath (OsPath, isAbsolute, takeDirectory)
import TypeScript.Config (Declared (..), DeclaredPaths (..), TsConfig, effectiveConfig, pathMappings)
import TypeScript.Config.Dto (CompilerOptionsDto (..), ExtendsDto (..), TsConfigDto (..), parseTsConfigJson)

{- | The ways a chain can fail to load.

Every one of them carries the chain that led to the offending file, root-most
first, because a config three @extends@ deep is otherwise a treasure hunt.
-}
data TsConfigLoadError
    = TsConfigUnreadable !AbsPath ![AbsPath]
    | TsConfigUnparseable !AbsPath ![AbsPath] !Text
    | TsConfigCycle !AbsPath ![AbsPath]
    | TsConfigExtendsNotText !AbsPath ![AbsPath]
    deriving (Show, Eq)

{- | An @extends@ naming a package rather than a path, which Deslop does not
resolve. Reported so that a project keeping its aliases in a shared config
package learns why they look unresolved, instead of drowning in false problems.
-}
data SkippedExtends = SkippedExtends
    { specifier :: !Text
    , inFile :: !AbsPath
    }
    deriving (Show, Eq)

loadTsConfig ::
    (RoFileSystem :> es) =>
    AbsPath ->
    Eff es (Either TsConfigLoadError (TsConfig, [SkippedExtends]))
loadTsConfig root = runErrorNoCallStack $ do
    canonical <- fsMkAbsolute root.osPath
    (declared, skipped) <- chainFrom [] canonical
    pure (effectiveConfig (dirOf canonical) (fold declared), skipped)

{- | Everything the config at this path declares, and everything it extends,
bases first.
-}
chainFrom ::
    ( RoFileSystem :> es
    , Error TsConfigLoadError :> es
    ) =>
    [AbsPath] -> AbsPath -> Eff es ([Declared], [SkippedExtends])
chainFrom chain path
    | path `elem` chain = throwError $ TsConfigCycle path chain
    | otherwise = do
        exists <- fsFileExists path
        unless exists . throwError $ TsConfigUnreadable path chain
        bytes <- fsReadFile path
        dto <- case parseTsConfigJson bytes of
            Left err -> throwError $ TsConfigUnparseable path chain err
            Right d -> pure d
        own <- declaredBy path dto.compilerOptions
        (inherited, skipped) <- extendedBy chain path dto.extends
        pure (inherited <> [own], skipped)

{- | What a config inherits, given the trail that led to it - which grows by
the config itself before its own bases are walked.
-}
extendedBy ::
    ( RoFileSystem :> es
    , Error TsConfigLoadError :> es
    ) =>
    [AbsPath] -> AbsPath -> Maybe ExtendsDto -> Eff es ([Declared], [SkippedExtends])
extendedBy _ _ Nothing = pure mempty
extendedBy chain path (Just ExtendsMalformed) = throwError $ TsConfigExtendsNotText path chain
extendedBy chain path (Just (ExtendsOne t)) = extendedByAll chain path [t]
extendedBy chain path (Just (ExtendsMany ts)) = extendedByAll chain path ts

{- | The bases in declaration order, so that a later entry of an array
@extends@ ends up further right in the fold and therefore wins.
-}
extendedByAll ::
    ( RoFileSystem :> es
    , Error TsConfigLoadError :> es
    ) =>
    [AbsPath] -> AbsPath -> [Text] -> Eff es ([Declared], [SkippedExtends])
extendedByAll chain path = fmap fold . traverse extendedByOne
  where
    extendedByOne t = case extendsTarget (dirOf path) t of
        Specifier spec -> pure ([], [SkippedExtends {specifier = spec, inFile = path}])
        ConfigPath p -> fsMkAbsolute p >>= chainFrom (chain <> [path])

-- | What one file states for itself, with its own directory baked in.
declaredBy ::
    (RoFileSystem :> es) =>
    AbsPath -> Maybe CompilerOptionsDto -> Eff es Declared
declaredBy _ Nothing = pure mempty
declaredBy path (Just opts) = do
    base <- traverse absoluteToDir opts.baseUrl
    pure
        Declared
            { baseUrl = Last base
            , paths = Last $ DeclaredPaths dir . pathMappings <$> opts.paths
            }
  where
    dir = dirOf path
    absoluteToDir = fsMkAbsolute . withAbsBaseSafe dir . encodeOsPath

-- | What an @extends@ value names.
data ExtendsTarget
    = ConfigPath !OsPath
    | -- | A package, resolved through @node_modules@ by the compiler and not by us.
      Specifier !Text
    deriving (Show, Eq)

{- | Where an @extends@ points, given the directory of the config that wrote it.

A value is a path when it is explicitly relative or rooted, exactly as the
compiler decides it; anything else is a package specifier. TypeScript appends
@.json@ to a path that does not already end in it, so a config may be extended
as @"./base"@.
-}
extendsTarget :: AbsPath -> Text -> ExtendsTarget
extendsTarget dir t
    | isExplicitlyRelative t = ConfigPath $ withAbsBaseSafe dir encoded
    | isRooted t = ConfigPath encoded
    | otherwise = Specifier t
  where
    encoded = encodeOsPath . withJsonExtension $ t

    isExplicitlyRelative :: Text -> Bool
    isExplicitlyRelative x = any (`T.isPrefixOf` x) ["./", "../", ".\\", "..\\"]

    -- A leading separator counts on its own: Windows reads "/shared" as
    -- drive-relative rather than absolute, and the compiler still takes it for
    -- a path rather than a package.
    isRooted :: Text -> Bool
    isRooted x = isAbsolute encoded || any (`T.isPrefixOf` x) ["/", "\\"]

    withJsonExtension :: Text -> Text
    withJsonExtension x
        | ".json" `T.isSuffixOf` x = x
        | otherwise = x <> ".json"

dirOf :: AbsPath -> AbsPath
dirOf = absPathUnsafe . takeDirectory . (.osPath)

renderTsConfigLoadError :: ProjectRoot -> TsConfigLoadError -> Text
renderTsConfigLoadError root (TsConfigUnreadable path chain) =
    "TS config not found: " <> quoted root path <> extendedFrom root chain
renderTsConfigLoadError root (TsConfigUnparseable path chain err) =
    "Could not parse " <> quoted root path <> ": " <> err <> extendedFrom root chain
renderTsConfigLoadError root (TsConfigCycle path chain) =
    "Circular \"extends\": " <> quoted root path <> " extends itself" <> extendedFrom root chain
renderTsConfigLoadError root (TsConfigExtendsNotText path chain) =
    "Invalid \"extends\" in "
        <> quoted root path
        <> ": expected a string or an array of strings"
        <> extendedFrom root chain

renderSkippedExtends :: ProjectRoot -> SkippedExtends -> Text
renderSkippedExtends root skipped =
    "Ignoring \"extends\": \""
        <> skipped.specifier
        <> "\" in "
        <> quoted root skipped.inFile
        <> ".\n"
        <> "Deslop resolves only relative and rooted paths, not package specifiers,\n"
        <> "so any path alias declared in that package is not applied."

-- | The @extends@ trail that reached a file, silent when there was none.
extendedFrom :: ProjectRoot -> [AbsPath] -> Text
extendedFrom _ [] = ""
extendedFrom root chain =
    "\n   extended from: " <> T.intercalate " -> " (quoted root <$> chain)

{- | A config file named from the project root. A base outside the project is
spelled with @..@ rather than absolutely, so the message reads the same on
every machine that runs the check.
-}
quoted :: ProjectRoot -> AbsPath -> Text
quoted root path = "'" <> decodeOsPath (relativePathTo root path).osPath <> "'"
