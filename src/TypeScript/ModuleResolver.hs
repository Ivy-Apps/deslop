{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolver (
    ModuleId (..),
    reverseResolve,
    reverseResolveImport,
    resolve,
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

{- | Resolves a TypeScript file's absolute path to a logical 'ModuleId'.

This function performs a "reverse resolution" to determine how a file
should be imported. It processes the path in the following order:
1. Strips the file extension from the absolute path.
2. Calculates the module's path relative to the 'TsConfig' @baseUrl@.
3. Iterates through the TSConfig @paths@ mappings to find an applicable alias.

If a valid path mapping is found, it substitutes the captured path into the
corresponding alias key. If no mappings match, it falls back to the path
relative to the configuration's base URL.

Example:
/home/repo/src/lib/util.tsx -> \@/lib/util (if alias mapped) OR src/lib/util
-}
reverseResolve :: (Reader TsConfig :> es) => AbsPath -> Eff es ModuleId
reverseResolve absFilePath = do
    cfg <- ask @TsConfig
    let noExtAbsFp = dropExtension absFilePath.osPath
    -- src/lib/util
    let (rawRelToCfg, _) = dropCommonPre (decodeOsPath noExtAbsFp, decodeOsPath cfg.baseUrl.osPath)
    let moduleRelToCfg = fromMaybe rawRelToCfg $ T.stripPrefix "/" rawRelToCfg
    pure . ModuleId . fromMaybe moduleRelToCfg $ applyPathMapping cfg.paths moduleRelToCfg
  where
    applyPathMapping :: [PathMapping] -> Text -> Maybe Text
    applyPathMapping [] _ = Nothing
    applyPathMapping (x : xs) moduleRelToCfg
        | Just found <- matchValues (toList x.values) moduleRelToCfg = case found of
            ExactMatch -> case x.key of
                (KeyPattern (Exact t)) -> Just t
                (KeyPattern (Wildcard pre suff)) -> Just (pre <> suff)
            WildcardMatch capture -> case x.key of
                (KeyPattern (Exact _)) -> applyPathMapping xs moduleRelToCfg
                (KeyPattern (Wildcard pre suff)) -> Just (pre <> capture <> suff)
        | otherwise = applyPathMapping xs moduleRelToCfg

    matchValues :: [ValuePattern] -> Text -> Maybe Match
    matchValues [] _ = Nothing
    matchValues (ValuePattern p : ps) t
        | Just found <- match p t = Just found
        | otherwise = matchValues ps t

reverseResolveImport ::
    ( RoFileSystem :> es
    , Reader TsConfig :> es
    ) =>
    AbsPath -> ModuleId -> Eff es ModuleId
reverseResolveImport _modulePath _importTarget = pure (ModuleId "")

resolve :: (RoFileSystem :> es, Reader TsConfig :> es) => ModuleId -> Eff es AbsPath
resolve _ = pure $ absPathUnsafe [osp|wip|]

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
