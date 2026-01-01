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
import Effectful (Eff, runEff, type (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, runReader)
import Effects.FileSystem (FileSystem, isDirectory, listDirectory, readFileBS, runFileSystemIO, writeFileBS)
import System.FilePath
import TypeScript.AST
import TypeScript.Config (TsConfig (TsConfig), parseTsConfig)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)

data DeslopError
    = ConfigParseError FilePath
    deriving (Show, Eq)

type ProjectPath = FilePath

deslopProject :: (FileSystem :> es, Error DeslopError :> es) => ProjectPath -> Eff es ()
deslopProject projPath = do
    let tsCfgPath = projPath </> "tsconfig.json"
    cfgContent <- readFileBS tsCfgPath
    cfg <- maybe (throwError $ ConfigParseError tsCfgPath) pure (parseTsConfig cfgContent)
    files <- getTsFiles projPath
    runReader @TsConfig cfg $ forM_ files $ \f -> do
        deslopFile f

getTsFiles :: (FileSystem :> es) => FilePath -> Eff es [FilePath]
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
    (FileSystem :> es, Reader TsConfig :> es) =>
    FilePath -> Eff es ()
deslopFile src = readFileBS src >>= removeSlop src >>= writeFileBS src

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
    res <-
        runEff . runFileSystemIO . runErrorNoCallStack @DeslopError $
            deslopProject projPath
    case res of
        Left err -> putStrLn $ "❌ Error: " ++ show err
        Right _ -> putStrLn "Deslop successful ✅"