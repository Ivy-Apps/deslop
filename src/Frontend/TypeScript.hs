{-# LANGUAGE QuasiQuotes #-}

module Frontend.TypeScript () where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop.Error (DeslopError (..))
import Deslop.Module (Module)
import Deslop.Problem.Baseline (Baseline)
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effects.CLI (CLI, LogStyle (..), cliLog)
import Effects.FileSystem (RoFileSystem, WrFileSystem, fsFileExists, fsReadFile, fsWriteFile)
import Effects.ReportProblem (ReportProblem)
import FileSystem.Path (AbsPath (..), ProjectRelativePath (..), ProjectRoot (..), decodeOsPath, relativePathTo, withAbsBaseUnsafe)
import Frontend.Class (CompilerFrontend (..))
import Git.Ignore (GitIgnore)
import Params (Command (..))
import Renderable (Renderable (..))
import System.OsPath (osp)
import TypeScript.CST (TsProgram (..))
import TypeScript.Config (TsConfig)
import TypeScript.Config.Loader (loadTsConfig, renderSkippedExtends, renderTsConfigLoadError)
import TypeScript.Iterator (getTsFiles)
import TypeScript.Lint.RelativeSpecifiers (noRelativeSpecifiers)
import TypeScript.Module (parseModule)
import TypeScript.Parser (TsFile (..), parseTs)

tsconfigPath :: ProjectRoot -> AbsPath
tsconfigPath (ProjectRoot p) = withAbsBaseUnsafe p [osp|tsconfig.json|]

data TsFrontend = TsFrontend

instance CompilerFrontend TsFrontend where
    detect root = fsFileExists (tsconfigPath root) >>= pure . bool Nothing (Just TsFrontend)
    label _ = "TypeScript"
    pipeline _ cmd = do
        pRoot <- ask @ProjectRoot
        cfg <- tsConfig pRoot
        gitIgnore <- ask @GitIgnore
        files <- getTsFiles gitIgnore pRoot.path
        runReader @TsConfig cfg $
            pooledMapConcurrentlyN 32 (deslopFile cmd) files

deslopFile ::
    ( RoFileSystem :> es
    , WrFileSystem :> es
    , Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , Reader Baseline :> es
    , CLI :> es
    , ReportProblem :> es
    ) =>
    Command ->
    AbsPath ->
    Eff es (Either String Module)
deslopFile cmd src = do
    c <- fsReadFile src
    cstRes <- lintFile src c
    let c' = either (const c) renderProgram cstRes
    when (c /= c' && cmd == FixC) $ do
        fsWriteFile src c'
        projectRoot <- ask @ProjectRoot
        cliLog Change $
            "  modified  " <> decodeOsPath (relativePathTo projectRoot src).osPath
    traverse parseModule cstRes
  where
    renderProgram = TE.encodeUtf8 . render . (.cst)

lintFile ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , Reader Baseline :> es
    , ReportProblem :> es
    , RoFileSystem :> es
    ) =>
    AbsPath ->
    ByteString ->
    Eff es (Either String TsProgram)
lintFile p c =
    traverse deslop . parseTs $
        TsFile {path = p, content = TE.decodeUtf8 c}
  where
    deslop = foldr (>=>) pure [noRelativeSpecifiers]

tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    , CLI :> es
    ) =>
    ProjectRoot ->
    Eff es TsConfig
tsConfig projectRoot = do
    res <- loadTsConfig $ withAbsBaseUnsafe projectRoot.path [osp|tsconfig.json|]
    case res of
        Left err -> throwError . FrontendError . renderTsConfigLoadError projectRoot $ err
        Right (cfg, skipped) -> cfg <$ traverse_ logSkipped skipped
  where
    logSkipped = cliLog Warning . ("WARNING: " <>) . renderSkippedExtends projectRoot
