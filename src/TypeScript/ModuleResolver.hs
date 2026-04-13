{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolver (
    ModuleId (..),
    encode,
    encodeImport,
    decode,
) where

import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe)
import System.OsPath (osp)
import TypeScript.Config (TsConfig (..))

newtype ModuleId = ModuleId Text deriving stock (Show, Eq)

encode :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> Eff es ModuleId
encode _filePath = do
    cfg <- ask @TsConfig
    let _x = cfg.baseUrl
    pure $ ModuleId ""

encodeImport :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> AbsPath -> Eff es ModuleId
encodeImport _modulePath _importTarget = pure (ModuleId "")

decode :: (RoFileSystem :> es, Reader TsConfig :> es) => ModuleId -> Eff es AbsPath
decode _ = pure $ absPathUnsafe [osp|wip|]
