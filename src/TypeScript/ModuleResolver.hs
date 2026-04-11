{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolver (
    ModuleId (..),
    encode,
    encodeImport,
    decode,
) where

import Effectful (Eff, (:>))
import Effects.FileSystem (AbsPath, RoFileSystem, absPathUnsafe)
import System.OsPath (osp)

newtype ModuleId = ModuleId Text deriving stock (Show, Eq)

encode :: (RoFileSystem :> es) => AbsPath -> Eff es ModuleId
encode _ = pure (ModuleId "")

encodeImport :: (RoFileSystem :> es) => AbsPath -> AbsPath -> Eff es ModuleId
encodeImport _modulePath _importTarget = pure (ModuleId "")

decode :: (RoFileSystem :> es) => ModuleId -> Eff es AbsPath
decode _ = pure $ absPathUnsafe [osp|wip|]
