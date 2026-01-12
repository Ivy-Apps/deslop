module Deslop (
    deslopFile,
    deslopProject,
    runDeslop,
    DeslopError (..),
    Params (..),
) where

import Control.Monad (forM_, (>=>))
import Data.Bool
import Data.ByteString (ByteString)
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.Imports (importAliases)
import Effectful (Eff, liftIO, runEff, type (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, runReader)
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
import System.Console.ANSI
import System.FilePath
import Text.Printf (printf)
import TypeScript.AST
import TypeScript.Config (TsConfig, parseTsConfig)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)

data Params = Params
    { projectPath :: FilePath
    , imports :: Bool
    , comments :: Bool
    , modified :: Bool
    }
    deriving (Show, Eq)

data DeslopError
    = TsConfigNotFoundError FilePath
    | TsConfigParseError FilePath
    deriving (Show, Eq)

deslopProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , Error DeslopError :> es
    , CLILog :> es
    ) =>
    Params -> Eff es ()
deslopProject params = do
    let projPath = params.projectPath
    cfg <- tsConfig projPath
    files <- getTsFiles projPath
    if params.modified
        then do
            mFiles <- modifiedFiles
            runReader @TsConfig cfg $ forM_ (intersect mFiles files) deslopFile
        else
            runReader @TsConfig cfg $ forM_ files deslopFile

tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    ) =>
    FilePath -> Eff es TsConfig
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
    FilePath -> Eff es ()
deslopFile src = do
    c <- readFileBS src
    c' <- removeSlop src c
    if c /= c'
        then do
            writeFileBS src c'
            logModification src
        else pure ()

removeSlop ::
    (Reader TsConfig :> es) =>
    FilePath -> ByteString -> Eff es ByteString
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
    setSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
    putStrLn $ "🚀 Deslopping project: " <> params.projectPath
    setSGR [Reset]
    putStrLn "─────────────────────────────────────────"
    putStrLn "Changelog:"
    runEff
        . runFileSystemIO
        . runCLILog
        . runGit
        $ do
            res <- runErrorNoCallStack @DeslopError (deslopProject params)
            liftIO $ putStrLn "─────────────────────────────────────────"
            case res of
                Left err -> liftIO $ do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn $ "❌ Error: " <> humanReadable err
                    setSGR [Reset]
                Right _ -> do
                    logSummary
                    end <- liftIO $ getCurrentTime
                    let diff = diffUTCTime end start
                    let seconds = realToFrac diff :: Double
                    liftIO $
                        if seconds < 1
                            then printf "⏱  Finished in %.2fms\n" (seconds * 1000)
                            else printf "⏱  Finished in %.2fs\n" seconds

humanReadable :: DeslopError -> String
humanReadable (TsConfigNotFoundError path) =
    "tsconfig.json not found in '" <> path <> "'"
humanReadable (TsConfigParseError path) =
    "Could not parse TS config, check: '" <> path <> "'"
