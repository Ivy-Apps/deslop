{-# LANGUAGE QuasiQuotes #-}

{- | TypeScript as a 'CompilerFrontend': a project with a @tsconfig.json@ at its
root, whose @.ts@ and @.tsx@ files lower to one 'Deslop.Module.Module' each.
-}
module Frontend.TypeScript (
    TsFrontend (..),
    TsParseError (..),
) where

import Data.Text.Encoding qualified as TE
import Deslop.Error (DeslopError (..))
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful (Eff, type (:>))
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (runReader)
import Effects.CLI (CLI, LogStyle (..), cliLog)
import Effects.FileSystem (RoFileSystem, fsFileExists)
import FileSystem.Path (AbsPath, ProjectRoot (..), withAbsBaseUnsafe)
import Frontend.TypeScript.Config (TsConfig)
import Frontend.TypeScript.Config.Loader (loadTsConfig, renderSkippedExtends, renderTsConfigLoadError)
import Frontend.TypeScript.CST (TsProgram (cst))
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

instance CompilerFrontend TsFrontend where
    type Config TsFrontend = TsConfig
    type Unit TsFrontend = TsProgram
    type ParseError TsFrontend = TsParseError

    detect projectRoot = bool Nothing (Just TsFrontend) <$> fsFileExists (tsConfigPath projectRoot)
    label _ = "TypeScript"
    lookingFor _ = "TypeScript (tsconfig.json)"
    codeFence _ = CodeFence "ts"
    builtInRules _ = relativeSpecifierRules
    ecosystemIgnored _ = jsEcosystemIgnored
    loadConfig _ = tsConfig
    discover _ _ projectRoot gitIgnore = getTsFiles gitIgnore projectRoot.path
    parse _ _ path = first TsParseError . parseTs . TsFile path . TE.decodeUtf8
    render _ = TE.encodeUtf8 . Renderable.render . (.cst)
    lint _ cfg = runReader cfg . noRelativeSpecifiers
    lower _ cfg = runReader cfg . pooledMapConcurrentlyN 32 parseModule

tsConfigPath :: ProjectRoot -> AbsPath
tsConfigPath projectRoot = withAbsBaseUnsafe projectRoot.path [osp|tsconfig.json|]

{- | The project's effective TypeScript configuration, @extends@ chain and all.

An @extends@ Deslop declines to follow is a warning rather than a failure: the
project is not broken, we simply cannot see what that package declares.
-}
tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    , CLI :> es
    ) =>
    ProjectRoot ->
    Eff es TsConfig
tsConfig projectRoot = do
    res <- loadTsConfig $ tsConfigPath projectRoot
    case res of
        Left err -> throwError . FrontendError . renderTsConfigLoadError projectRoot $ err
        Right (cfg, skipped) -> cfg <$ traverse_ logSkipped skipped
  where
    logSkipped = cliLog Warning . ("WARNING: " <>) . renderSkippedExtends projectRoot
