{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolver (
    ModuleId (..),
    encode,
    encodeImport,
    decode,
    match,
) where

import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe, decodeOsPath)
import System.OsPath (osp)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import Utils (dropCommonPre)

newtype ModuleId = ModuleId Text deriving stock (Show, Eq)

data ExactPathMapping = ExactPathMapping
    { key :: KeyPattern
    , value :: ValuePattern
    }
    deriving (Show, Eq)

encode :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> Eff es ModuleId
encode absFilePath = do
    cfg <- ask @TsConfig
    let (_fpRelToCfg, _) = dropCommonPre (decodeOsPath absFilePath.osPath, decodeOsPath cfg.baseUrl.osPath)

    pure $ ModuleId ""
  where
    _findMatchingPath :: (RoFileSystem :> es) => Text -> [PathMapping] -> Eff es (Maybe KeyPattern)
    _findMatchingPath _ [] = pure Nothing
    _findMatchingPath _fpRelToCfg ((PathMapping _k _vs) : _xs) = do
        pure Nothing

encodeImport :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> AbsPath -> Eff es ModuleId
encodeImport _modulePath _importTarget = pure (ModuleId "")

decode :: (RoFileSystem :> es, Reader TsConfig :> es) => ModuleId -> Eff es AbsPath
decode _ = pure $ absPathUnsafe [osp|wip|]

match :: Pattern -> Text -> Bool
match (Exact p) t = p == t
match (Wildcard pre suff) t =
    pre `T.isPrefixOf` t
        && suff `T.isSuffixOf` t
        && T.length t > T.length pre + T.length suff
