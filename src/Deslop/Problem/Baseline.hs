{-# LANGUAGE QuasiQuotes #-}

module Deslop.Problem.Baseline (
    loadBaseline,
    loadBaselineFromFile,
    applyBaseline,
    inBaseline,
    saveBaseline,
    Baseline (..),
) where

import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Yaml qualified as YAML
import Deslop.Problem (Problem (..), ProblemId (..), problemId)
import Effectful (Eff)
import Effectful.Internal.Effect ((:>))
import Effects.FileSystem (
    RoFileSystem,
    WrFileSystem,
    fsMkDirP,
    fsReadFileIfExists,
    fsWriteFile,
 )
import FileSystem.Path (AbsPath, withAbsBaseUnsafe)
import System.OsPath (OsPath, osp)

newtype Baseline = Baseline (HashSet ProblemId)
    deriving stock (Show, Eq)
    deriving newtype (Semigroup, Monoid)

applyBaseline :: Baseline -> [Problem] -> [Problem]
applyBaseline baseline = filter (not . inBaseline baseline)

inBaseline :: Baseline -> Problem -> Bool
inBaseline (Baseline bs) = (`HS.member` bs) . problemId

baselinePath :: OsPath
baselinePath = [osp|deslop/baseline.yaml|]

loadBaseline :: (RoFileSystem :> es) => AbsPath -> Eff es Baseline
loadBaseline projectPath = loadBaselineFromFile (withAbsBaseUnsafe projectPath baselinePath)

loadBaselineFromFile :: (RoFileSystem :> es) => AbsPath -> Eff es Baseline
loadBaselineFromFile fp = do
    maybe mempty (fromRight mempty . parseBasline) <$> fsReadFileIfExists fp

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
    ids = sort . HS.toList . HS.fromList . fmap ((.text) . problemId) $ problems
