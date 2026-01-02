module Deslop (
    deslopFile,
    deslopProject,
    runDeslop,
    DeslopError (..),
) where

import Control.Monad (forM_, (>=>))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as T
import Data.Traversable
import Deslop.Imports (importAliases)
import Effectful (Eff, liftIO, runEff, type (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, runReader)
import Effects.CLILog
import Effects.FileSystem (
    RoFileSystem,
    WrFileSystem,
    isDirectory,
    listDirectory,
    readFileBS,
    runFileSystemIO,
    writeFileBS,
 )
import System.Console.ANSI
import System.FilePath
import TypeScript.AST
import TypeScript.Config (TsConfig, parseTsConfig)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)

data DeslopError
    = ConfigParseError FilePath
    deriving (Show, Eq)

type ProjectPath = FilePath

deslopProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Error DeslopError :> es
    , CLILog :> es
    ) =>
    ProjectPath -> Eff es ()
deslopProject projPath = do
    let tsCfgPath = projPath </> "tsconfig.json"
    cfgContent <- readFileBS tsCfgPath
    cfg <- maybe (throwError $ ConfigParseError tsCfgPath) pure (parseTsConfig cfgContent)
    files <- getTsFiles projPath
    runReader @TsConfig cfg $ forM_ files deslopFile

getTsFiles :: (RoFileSystem :> es) => FilePath -> Eff es [FilePath]
getTsFiles dir = do
    entries <- listDirectory dir
    paths <- forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- isDirectory path
        if isDir
            then
                if entry `elem` ["node_modules", ".git", "dist", ".next"]
                    then pure []
                    else getTsFiles path
            else pure [path | takeExtension path `elem` [".ts", ".tsx"]]
    pure $ concat paths

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
            TsFile {path = p, content = T.decodeUtf8 c}
    deslop = foldr (>=>) pure [importAliases]
    render = T.encodeUtf8 . renderAst . (.ast)

runDeslop :: ProjectPath -> IO ()
runDeslop projPath = do
    setSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
    putStrLn $ "🚀 Deslopping project: " <> projPath
    setSGR [Reset]
    putStrLn "─────────────────────────────────────────"
    putStrLn "Changelog:"
    runEff
        . runFileSystemIO
        . runCLILog
        $ do
            res <- runErrorNoCallStack @DeslopError (deslopProject projPath)
            liftIO $ putStrLn "─────────────────────────────────────────"
            case res of
                Left err -> liftIO $ do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn $ "❌ Error: " <> show err
                    setSGR [Reset]
                Right _ -> logSummary