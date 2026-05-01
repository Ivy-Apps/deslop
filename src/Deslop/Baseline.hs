{-# LANGUAGE QuasiQuotes #-}

module Deslop.Baseline (
    loadBaseline,
    loadBaselineFromFile,
) where

import Data.HashSet qualified as HS
import Deslop.Problem (ProblemId)
import Effectful (Eff)
import Effectful.Internal.Effect ((:>))
import Effects.FileSystem (AbsPath, RoFileSystem, fsFileExists, fsMkAbsolute, fsReadFile)
import System.OsPath (OsPath, osp)
import Utils (todo)

newtype Baseline = Baseline (HashSet ProblemId) deriving (Show, Eq)

emptyBaseline :: Baseline
emptyBaseline = Baseline HS.empty

baselinePath :: OsPath
baselinePath = [osp|deslop/baseline.yaml|]

loadBaseline :: (RoFileSystem :> es) => Eff es Baseline
loadBaseline = fsMkAbsolute baselinePath >>= loadBaselineFromFile

loadBaselineFromFile :: (RoFileSystem :> es) => AbsPath -> Eff es Baseline
loadBaselineFromFile fp = do
    exists <- fsFileExists fp
    if exists
        then
            fsReadFile fp >>= pure . fromRight emptyBaseline . parseBasline
        else pure emptyBaseline

parseBasline :: ByteString -> Either Text Baseline
parseBasline = todo
