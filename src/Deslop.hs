module Deslop (
    deslopFile,
    deslopProject,
    runDeslop,
    DeslopError (..),
) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as T
import Deslop.Imports (importAliases)
import Effectful (Eff, MonadIO (liftIO), runEff, type (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, runReader)
import Effects.FileSystem (FileSystem, readFileBS, runFileSystemIO, writeFileBS)
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
    cfg <- parseTsConfig <$> readFileBS (projPath </> "tsconfig.json")
    pure ()

deslopFile ::
    (FileSystem :> es, Reader TsConfig :> es) =>
    FilePath -> FilePath -> Eff es ()
deslopFile src dst = readFileBS src >>= removeSlop src >>= writeFileBS dst

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