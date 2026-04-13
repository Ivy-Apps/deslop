{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolver (
    ModuleId (..),
    encode,
    encodeImport,
    decode,
    match,
    Match (..),
) where

import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (AbsPath (..), RoFileSystem, absPathUnsafe, decodeOsPath)
import System.OsPath (dropExtension, osp)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import Utils (dropCommonPre)

newtype ModuleId = ModuleId Text deriving stock (Show, Eq)

data ExactPathMapping = ExactPathMapping
    { key :: KeyPattern
    , value :: ValuePattern
    }
    deriving (Show, Eq)

-- home/repo/src/lib/util.tsx -> @/lib/util OR src/lib/util
encode :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> Eff es ModuleId
encode absFilePath = do
    cfg <- ask @TsConfig
    let noExtAbsFp = dropExtension absFilePath.osPath
    -- src/lib/util
    let (moduleRelToCfg, _) = dropCommonPre (decodeOsPath noExtAbsFp, decodeOsPath cfg.baseUrl.osPath)

    pure $ ModuleId ""
  where
    applyPathMapping :: [PathMapping] -> Text -> Maybe Text
    applyPathMapping [] _ = Nothing
    applyPathMapping (x : xs) moduleRelToCfg
        | Just found <- matchValues (toList x.values) moduleRelToCfg = case found of
            ExactMatch -> Nothing
            WildcardMatch capture -> Nothing
        | otherwise = applyPathMapping xs moduleRelToCfg

    matchValues :: [ValuePattern] -> Text -> Maybe Match
    matchValues [] _ = Nothing
    matchValues (ValuePattern p : ps) t
        | Just found <- match p t = Just found
        | otherwise = matchValues ps t

encodeImport :: (RoFileSystem :> es, Reader TsConfig :> es) => AbsPath -> AbsPath -> Eff es ModuleId
encodeImport _modulePath _importTarget = pure (ModuleId "")

decode :: (RoFileSystem :> es, Reader TsConfig :> es) => ModuleId -> Eff es AbsPath
decode _ = pure $ absPathUnsafe [osp|wip|]

data Match = ExactMatch | WildcardMatch Text deriving (Show, Eq)

match :: Pattern -> Text -> Maybe Match
match (Exact p) t
    | p == t = Just ExactMatch
    | otherwise = Nothing
match (Wildcard pre suff) t
    | T.length t >= T.length pre + T.length suff
    , Just rest <- T.stripPrefix pre t
    , Just capture <- T.stripSuffix suff rest =
        Just (WildcardMatch capture)
    | otherwise = Nothing
