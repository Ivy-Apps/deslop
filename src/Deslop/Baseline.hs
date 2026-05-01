{-# LANGUAGE QuasiQuotes #-}

module Deslop.Baseline (
    loadBaseline,
    loadBaselineFromFile,
    applyBaseline,
) where

import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Yaml (decodeEither')
import Deslop.Problem (Problem (..), ProblemId (..), problemId)
import Effectful (Eff)
import Effectful.Internal.Effect ((:>))
import Effects.FileSystem (AbsPath, RoFileSystem, fsFileExists, fsMkAbsolute, fsReadFile)
import System.OsPath (OsPath, osp)

newtype Baseline = Baseline (HashSet ProblemId) deriving (Show, Eq)

applyBaseline :: Baseline -> [Problem] -> [Problem]
applyBaseline (Baseline bs) ps = filter (not . (`HS.member` bs) . problemId) ps

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
parseBasline bs =
    first (T.pack . show) (decodeEither' @[Text] bs)
        <&> (Baseline . HS.fromList . fmap ProblemId)
