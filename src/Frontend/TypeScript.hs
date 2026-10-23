{-# LANGUAGE QuasiQuotes #-}

{- | TypeScript as a 'CompilerFrontend': a project with a @tsconfig.json@ at its
root, whose @.ts@ and @.tsx@ files lower to one 'Deslop.Module.Module' each.
-}
module Frontend.TypeScript (
    TsFrontend (..),
    TsUnit (..),
    TsLowerError (..),
) where

import Data.Text.Encoding qualified as TE
import Deslop.Error (Warning (..))
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (within)
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful (Eff, type (:>))
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Reader.Static (ask, runReader)
import Effects.Concurrency (poolSize)
import Effects.FileSystem (RoFileSystem, fsFileExists)
import FileSystem.Path (AbsPath, ProjectRelativePath, ProjectRoot (..), portablePath, relativePathTo, withAbsBaseUnsafe)
import Frontend.TypeScript.CST (TsProgram (..))
import Frontend.TypeScript.Config (TsConfig)
import Frontend.TypeScript.Config.Loader (TsConfigLoadError, loadTsConfig, renderSkippedExtends, renderTsConfigLoadError)
import Frontend.TypeScript.Ignore (jsEcosystemIgnored)
import Frontend.TypeScript.Iterator (getTsFiles)
import Frontend.TypeScript.Lint.RelativeSpecifiers (noRelativeSpecifiers, relativeSpecifierRules)
import Frontend.TypeScript.Module (parseModule)
import Frontend.TypeScript.Parser (TsFile (..), parseTs)
import Renderable (Renderable)
import Renderable qualified
import System.OsPath (osp)

-- | Witness that the project is a TypeScript one.
data TsFrontend = TsFrontend
    deriving stock (Show, Eq)

{- | One TypeScript file as read: its syntax, or - when its bytes are not UTF-8 -
the bytes themselves, kept whole so that rendering still gives them back and
the file is still a module 'lower' sees.
-}
data TsUnit
    = Readable TsProgram
    | NotUtf8 AbsPath ByteString
    deriving stock (Show, Eq)

-- | A file whose bytes are not UTF-8: its module exists, but its imports are unknown.
newtype TsLowerError = ImportsNotRead ProjectRelativePath
    deriving stock (Show, Eq)

instance Renderable TsLowerError where
    render (ImportsNotRead path) = portablePath path <> " is not UTF-8, so its imports were not read."

data TsConfigError = TsConfigError !ProjectRoot !TsConfigLoadError
    deriving stock (Show, Eq)

instance Renderable TsConfigError where
    render (TsConfigError pRoot e) = renderTsConfigLoadError pRoot e

instance CompilerFrontend TsFrontend where
    type Config TsFrontend = TsConfig
    type ConfigError TsFrontend = TsConfigError
    type Unit TsFrontend = TsUnit
    type LowerError TsFrontend = TsLowerError

    detect = fmap (bool Nothing (Just TsFrontend)) . fsFileExists . tsConfigPath
    label _ = "TypeScript"
    lookingFor _ = "TypeScript (tsconfig.json)"
    codeFence _ = CodeFence "ts"
    builtInRules _ = relativeSpecifierRules
    ecosystemIgnored _ _ = jsEcosystemIgnored
    loadConfig _ = tsConfig
    discover _ _ projectRoot gitIgnore = getTsFiles gitIgnore projectRoot.path
    parse _ _ path bytes = case TE.decodeUtf8' bytes of
        Right content -> Readable . parseTs $ TsFile path content
        Left _ -> NotUtf8 path bytes
    render _ = \case
        Readable program -> TE.encodeUtf8 . Renderable.render $ program.cst
        NotUtf8 _ bytes -> bytes
    lint _ cfg = \case
        Readable program -> fmap (within onReadable) <$> runReader cfg (noRelativeSpecifiers program)
        NotUtf8 {} -> pure []
      where
        onReadable (Endo edit) = Endo $ \case
            Readable program -> Readable (edit program)
            unread@NotUtf8 {} -> unread
    lower _ cfg units = do
        projectRoot <- ask @ProjectRoot
        ms <- runReader cfg . pooledMapConcurrentlyN poolSize parseModule $ programOf <$> units
        pure (ms, [ImportsNotRead (relativePathTo projectRoot path) | NotUtf8 path _ <- units])
      where
        -- Still a module, named by its path; it is its imports nobody knows.
        programOf = \case
            Readable program -> program
            NotUtf8 path _ -> TsModule {path, cst = []}

tsConfigPath :: ProjectRoot -> AbsPath
tsConfigPath projectRoot = withAbsBaseUnsafe projectRoot.path [osp|tsconfig.json|]

tsConfig ::
    (RoFileSystem :> es) =>
    ProjectRoot -> Eff es (Either TsConfigError (TsConfig, [Warning]))
tsConfig projectRoot =
    bimap (TsConfigError projectRoot) (second mapWarnings)
        <$> loadTsConfig (tsConfigPath projectRoot)
  where
    mapWarnings = fmap (Warning . renderSkippedExtends projectRoot)
