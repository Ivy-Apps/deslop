{-# LANGUAGE QuasiQuotes #-}

module Deslop.Baseline (
    loadBaseline,
    loadBaselineFromFile,
    applyBaseline,
    inBaseline,
    saveBaseline,
    emptyBaseline,
    Baseline (..),
) where

import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Yaml qualified as YAML
import Deslop.Problem (Problem (..), ProblemId (..), problemId)
import Effectful (Eff)
import Effectful.Internal.Effect ((:>))
import Effects.FileSystem (AbsPath, RoFileSystem, WrFileSystem, fsFileExists, fsMkDirP, fsReadFile, fsWriteFile, withAbsBaseUnsafe)
import System.OsPath (OsPath, osp)

newtype Baseline = Baseline (HashSet ProblemId) deriving (Show, Eq)

applyBaseline :: Baseline -> [Problem] -> [Problem]
applyBaseline baseline = filter (not . inBaseline baseline)

inBaseline :: Baseline -> Problem -> Bool
inBaseline (Baseline bs) = (`HS.member` bs) . problemId

emptyBaseline :: Baseline
emptyBaseline = Baseline HS.empty

baselinePath :: OsPath
baselinePath = [osp|deslop/baseline.yaml|]

loadBaseline :: (RoFileSystem :> es) => AbsPath -> Eff es Baseline
loadBaseline projectPath = loadBaselineFromFile (withAbsBaseUnsafe projectPath baselinePath)

loadBaselineFromFile :: (RoFileSystem :> es) => AbsPath -> Eff es Baseline
loadBaselineFromFile fp = do
    exists <- fsFileExists fp
    if exists
        then
            fsReadFile fp >>= pure . fromRight emptyBaseline . parseBasline
        else pure emptyBaseline

parseBasline :: ByteString -> Either Text Baseline
parseBasline bs =
    first (T.pack . show) (YAML.decodeEither' @[Text] bs)
        <&> (Baseline . HS.fromList . fmap (ProblemId . T.strip))

saveBaseline :: (WrFileSystem :> es) => AbsPath -> [Problem] -> Eff es ()
saveBaseline projectPath problems = do
    fsMkDirP deslopDir
    fsWriteFile baselineFile (YAML.encode ids)
  where
    deslopDir = withAbsBaseUnsafe projectPath [osp|deslop|]
    baselineFile = withAbsBaseUnsafe projectPath baselinePath
    ids = map ((.text) . problemId) problems
