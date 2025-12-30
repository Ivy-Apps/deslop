module Deslop (deslopFile, runDeslop) where

import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as T
import Deslop.Imports (fixImports)
import Effectful (Eff, MonadIO (liftIO), runEff, type (:>))
import Effectful.Reader.Static (Reader, runReader)
import Effects.FileSystem (FileSystem, readFileBS, runFileSystemIO, writeFileBS)
import TypeScript.AST
import TypeScript.Config (TsConfig (TsConfig))
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)

deslopFile ::
    (FileSystem :> es, Reader TsConfig :> es) =>
    FilePath -> FilePath -> Eff es ()
deslopFile src dst = readFileBS src >>= removeSlop src >>= writeFileBS dst

removeSlop ::
    (Reader TsConfig :> es) =>
    FilePath -> ByteString -> Eff es ByteString
removeSlop p c = pipeline >>= pure . either (const c) id
  where
    pipeline :: (Reader TsConfig :> es) => Eff es (Either String ByteString)
    pipeline = case parseTs (TsFile {path = p, content = T.decodeUtf8 c}) of
        Right prog -> deslop prog >>= pure . Right . T.encodeUtf8 . renderAst . (.ast)
        Left e -> pure . Left $ e

    deslop :: (Reader TsConfig :> es) => TsProgram -> Eff es TsProgram
    deslop = fixImports

runDeslop :: IO ()
runDeslop = do
    runEff
        . runFileSystemIO
        . runReader (TsConfig [])
        $ do
            deslopFile "test/fixtures/typescript/01-imports.ts" "demo.ts"
            liftIO $ putStrLn "Deslop complete ✅"