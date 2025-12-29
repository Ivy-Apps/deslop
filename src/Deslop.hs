module Deslop (deslopFile, runDeslop) where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text.Encoding qualified as T
import Deslop.Imports (fixImports)
import Effectful (Eff, MonadIO (liftIO), runEff, type (:>))
import Effects.FileSystem (FileSystem, readFileBS, runFileSystemIO, writeFileBS)
import TypeScript.AST
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs, renderAst)

deslopFile :: (FileSystem :> es) => FilePath -> FilePath -> Eff es ()
deslopFile src dst = readFileBS src >>= writeFileBS dst . removeSlop src

removeSlop :: FilePath -> ByteString -> ByteString
removeSlop p c = case pipeline of
    Left _ -> c
    Right c' -> c'
  where
    pipeline :: Either String ByteString
    pipeline =
        parseTs (TsFile {path = p, content = T.decodeUtf8 c})
            >>= pure . T.encodeUtf8 . renderAst . (.ast) . deslop

    deslop :: TsProgram -> TsProgram
    deslop = fixImports

runDeslop :: IO ()
runDeslop = runEff $ runFileSystemIO $ do
    deslopFile "test/fixtures/typescript/input/01-comments.ts" "demo.ts"
    liftIO $ putStrLn "Deslop complete ✅"