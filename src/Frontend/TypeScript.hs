{-# LANGUAGE QuasiQuotes #-}

{- | TypeScript as a 'CompilerFrontend': a project with a @tsconfig.json@ at its
root, whose @.ts@ and @.tsx@ files lower to one 'Deslop.Module.Module' each.
-}
module Frontend.TypeScript (
    TsFrontend (..),
    TsParseError (..),
) where

import Data.Text.Encoding qualified as TE
import Deslop.Error (VoidError, Warning (..))
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful (Eff, type (:>))
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Reader.Static (runReader)
import Effects.Concurrency (poolSize)
import Effects.FileSystem (RoFileSystem, fsFileExists, fsIsSymlink, fsListDirectory)
import FileSystem.Path (AbsPath, ProjectRoot (..), withAbsBaseUnsafe)
import Frontend.TypeScript.CST (TsProgram (cst))
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

{- | Megaparsec's report on a file the lexer could not read, already rendered.
It starts at the line and column; the driver names the file.
-}
newtype TsParseError = TsParseError String
    deriving stock (Show, Eq)

instance Renderable TsParseError where
    render (TsParseError err) = toText err

data TsConfigError = TsConfigError !ProjectRoot !TsConfigLoadError
    deriving stock (Show, Eq)

instance Renderable TsConfigError where
    render (TsConfigError pRoot e) = renderTsConfigLoadError pRoot e

instance CompilerFrontend TsFrontend where
    type Config TsFrontend = TsConfig
    type ConfigError TsFrontend = TsConfigError
    type Unit TsFrontend = TsProgram
    type ParseError TsFrontend = TsParseError
    type LowerError TsFrontend = VoidError

    detect projectRoot = bool Nothing (Just TsFrontend) <$> hasTsConfig projectRoot
    label _ = "TypeScript"
    lookingFor _ = "TypeScript (tsconfig.json)"
    codeFence _ = CodeFence "ts"
    builtInRules _ = relativeSpecifierRules
    ecosystemIgnored _ _ = jsEcosystemIgnored
    loadConfig _ = tsConfig
    discover _ _ projectRoot gitIgnore = getTsFiles gitIgnore projectRoot.path
    parse _ _ path = first TsParseError . parseTs . TsFile path . TE.decodeUtf8
    render _ = TE.encodeUtf8 . Renderable.render . (.cst)
    lint _ cfg = runReader cfg . noRelativeSpecifiers
    lower _ cfg = do
        runReader cfg
            . pooledMapConcurrentlyN poolSize parseModule
            >=> (\ms -> pure (ms, []))

tsConfigPath :: ProjectRoot -> AbsPath
tsConfigPath projectRoot = withAbsBaseUnsafe projectRoot.path [osp|tsconfig.json|]

{- | Whether the root holds a @tsconfig.json@, even a link that points nowhere.

A dangling link is still a TypeScript project, just a broken one: 'loadConfig'
then names the config it could not find. Checking for the link itself needs it
to be listed first, as a link that is not there cannot be asked about.
-}
hasTsConfig :: (RoFileSystem :> es) => ProjectRoot -> Eff es Bool
hasTsConfig projectRoot = fsFileExists path >>= bool danglingLink (pure True)
  where
    path = tsConfigPath projectRoot
    danglingLink = fsListDirectory projectRoot.path >>= bool (pure False) (fsIsSymlink path) . elem path

tsConfig ::
    (RoFileSystem :> es) =>
    ProjectRoot -> Eff es (Either TsConfigError (TsConfig, [Warning]))
tsConfig projectRoot =
    bimap (TsConfigError projectRoot) (second mapWarnings)
        <$> loadTsConfig (tsConfigPath projectRoot)
  where
    mapWarnings = fmap (Warning . renderSkippedExtends projectRoot)
