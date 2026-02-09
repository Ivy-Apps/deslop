module Deslop (
    deslopFile,
    deslopProject,
    runDeslop,
    translateProject,
) where

import Control.Monad (forM_, when, (>=>))
import Data.Aeson
import Data.Bool
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.Imports (importAliases)
import Effectful (Eff, IOE, liftIO, runEff, type (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, runReader)
import Effects.AI
import Effects.CLILog
import Effects.FileSystem (
    RoFileSystem,
    WrFileSystem,
    fileExists,
    isDirectory,
    listDirectory,
    readFileBS,
    runFileSystemIO,
    writeFileBS,
 )
import Effects.Git
import GHC.Generics (Generic)
import System.Console.ANSI
import System.FilePath
import Text.Printf (printf)
import Translations.Manager
import Translations.Parser
import TypeScript.AST
import TypeScript.Config (TsConfig, parseTsConfig)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)
import Types
import UI

translateProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , CLILog :> es
    , AI :> es
    , Error TranslationsError :> es
    ) =>
    Params ->
    Eff es ()
translateProject params =
    readTranslations translationsPath
        >>= maybe handleReadError pipeline
  where
    pipeline ts = fixTranslations ts >>= either handleTranslateErorr writeTranslations
    writeTranslations = traverse_ writeTranslation . (.extra)
    writeTranslation (Translation l t) = writeFileBS (translationFile l) (TE.encodeUtf8 $ render t)

    translationFile l = translationsPath </> (T.unpack l <> ".json")
    translationsPath = params.projectPath </> "messages"

    handleReadError = throwError ParseTranslationsError
    handleTranslateErorr = throwError . TranslateError

deslopProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , Error DeslopError :> es
    , CLILog :> es
    ) =>
    Params ->
    Eff es ()
deslopProject params = do
    let projPath = params.projectPath
    cfg <- tsConfig projPath
    files <- getTsFiles projPath
    if params.modified
        then do
            mFiles <- map normalise <$> modifiedFiles
            runReader @TsConfig cfg $
                forM_ (mFiles `intersect` (normalise <$> files)) deslopFile
        else
            runReader @TsConfig cfg $ forM_ files deslopFile

tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    ) =>
    FilePath ->
    Eff es TsConfig
tsConfig projPath = loadConfig $ projPath </> "tsconfig.json"
  where
    loadConfig fp = fileExists fp >>= bool (handleMissing fp) (handleFound fp)
    handleFound fp = readFileBS fp >>= maybe (handleInvalid fp) pure . parseTsConfig

    handleMissing = throwError . TsConfigNotFoundError
    handleInvalid = throwError . TsConfigParseError

getTsFiles :: (RoFileSystem :> es) => FilePath -> Eff es [FilePath]
getTsFiles dir = listDirectory dir >>= fmap concat . traverse (processEntry dir)
  where
    processEntry root entry
        | entry `elem` ignored = pure []
        | otherwise = resolve $ root </> entry

    resolve path = isDirectory path >>= bool (tsOrEmpty path) (getTsFiles path)

    tsOrEmpty f = pure [f | takeExtension f `elem` [".ts", ".tsx"]]
    ignored = ["node_modules", ".git", "dist", ".next"]

deslopFile ::
    ( RoFileSystem :> es
    , WrFileSystem :> es
    , Reader TsConfig :> es
    , CLILog :> es
    ) =>
    FilePath ->
    Eff es ()
deslopFile src = do
    c <- readFileBS src
    c' <- removeSlop src c
    when (c /= c') $ do
        writeFileBS src c'
        logModification src

removeSlop ::
    (Reader TsConfig :> es) =>
    FilePath ->
    ByteString ->
    Eff es ByteString
removeSlop p c = fromMaybe c . either (const Nothing) Just <$> pipeline
  where
    pipeline =
        traverse (fmap render . deslop) . parseTs $
            TsFile {path = p, content = TE.decodeUtf8 c}
    deslop = foldr (>=>) pure [importAliases]
    render = TE.encodeUtf8 . renderAst . (.ast)

runDeslop :: Params -> IO ()
runDeslop params = do
    start <- getCurrentTime
    let s =
            Secrets
                { geminiApiKey = "TBD"
                }

    runEff
        . runFileSystemIO
        . runCLILog
        . runGit
        . runReader @Secrets s
        $ doWork params

    end <- liftIO $ getCurrentTime
    let diff = diffUTCTime end start
    let seconds = realToFrac diff :: Double
    printTime seconds

doWork ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , CLILog :> es
    , IOE :> es
    , Reader Secrets :> es
    ) =>
    Params ->
    Eff es ()
doWork params = do
    liftIO . printTitle $ "🚀 Deslopping project: " <> T.pack params.projectPath
    liftIO . putStrLn $ "Changelog:"
    res <- runErrorNoCallStack @DeslopError (deslopProject params)
    liftIO printDivider
    case res of
      Left err -> liftIO . printErr . humanReadable $ err
      Right _ -> logSummary
    liftIO printDivider
    liftIO . putStrLn $ "Translating..."
    res <- runAI . runErrorNoCallStack @TranslationsError $ translateProject params
    case res of
      Left err -> liftIO . printErr . T.pack $ show err
      Right _ -> liftIO . putStrLn $ "Translations success."
