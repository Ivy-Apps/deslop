module Deslop (deslopFile, deslopProject, runDeslop) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as T
import Deslop.Imports (importAliases)
import Effectful (Eff, MonadIO (liftIO), runEff, type (:>))
import Effectful.Reader.Static (Reader, runReader)
import Effects.FileSystem (FileSystem, readFileBS, runFileSystemIO, writeFileBS)
import TypeScript.AST
import TypeScript.Config (TsConfig (TsConfig))
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)

deslopProject :: (FileSystem :> es) => FilePath -> Eff es ()
deslopProject _ = return ()

deslopFile ::
    (FileSystem :> es, Reader TsConfig :> es) =>
    FilePath -> FilePath -> Eff es ()
deslopFile src dst = readFileBS src >>= removeSlop src >>= writeFileBS dst

removeSlop ::
    (Reader TsConfig :> es, FileSystem :> es) =>
    FilePath -> ByteString -> Eff es ByteString
removeSlop p c = fromMaybe c . either (const Nothing) Just <$> pipeline
  where
    pipeline =
        traverse (fmap render . deslop) . parseTs $
            TsFile {path = p, content = T.decodeUtf8 c}
    deslop = foldr (>=>) pure [importAliases]
    render = T.encodeUtf8 . renderAst . (.ast)

runDeslop :: IO ()
runDeslop = do
    runEff
        . runFileSystemIO
        . runReader (TsConfig [])
        $ do
            deslopFile "test/fixtures/typescript/01-imports.ts" "demo.ts"
            liftIO $ putStrLn "Deslop complete ✅"