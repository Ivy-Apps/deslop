module Deslop.Imports where

import Data.Bifunctor
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Reader.Static
import Effects.FileSystem
import TypeScript.AST
import TypeScript.Config

importAliases :: (Reader TsConfig :> es, FileSystem :> es) => TsProgram -> Eff es TsProgram
importAliases prog = do
    ast' <- traverse fixImport prog.ast
    pure prog {ast = ast'}
  where
    fixImport og@(Import _ t _) = do
        t' <- fixTarget t
        pure og {target = t'}
    fixImport x = pure x

    fixTarget t = do
        as <- asks @TsConfig (.paths)
        absT <- T.pack <$> absPath as t
        case useAlias as absT of
            Just absT' -> pure . fst $ dropCommonPre (absT', absT)
            Nothing -> pure t

    dropCommonPre :: (Text, Text) -> (Text, Text)
    dropCommonPre (x, y) = case T.commonPrefixes x y of
        Just (_, x', y') -> (x', y')
        Nothing -> (x, y)

    useAlias as fp = applyAlias <$> findAliasForPath as
      where
        applyAlias (ImportAlias a p) = T.replace p a fp
        findAliasForPath = find (\a -> a.path `T.isInfixOf` fp)

    absPath as = fullPath . T.unpack . reverseAlias as

    reverseAlias as fp =
        fromMaybe fp
            . fmap (\(ImportAlias l p) -> T.replace l p fp)
            . find ((`T.isInfixOf` fp) . (.label))
            $ as
