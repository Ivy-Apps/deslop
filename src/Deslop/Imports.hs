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
        absP <- T.pack <$> absPath as t
        let absAl = useAlias absP as
        pure . fst $ dropCommon (absAl, t)

    dropCommon :: (Text, Text) -> (Text, Text)
    dropCommon (x, y) = case T.commonPrefixes x y of
        Just (_, x', y') -> (x', y')
        Nothing -> (x, y)

    useAlias fp =
        fromMaybe fp
            . fmap (\(ImportAlias l p) -> T.replace p l fp)
            . find ((`T.isInfixOf` fp) . (.path))

    absPath as = fullPath . T.unpack . reverseAlias as

    reverseAlias as fp =
        fromMaybe fp
            . fmap (\(ImportAlias l p) -> T.replace l p fp)
            . find ((`T.isInfixOf` fp) . (.label))
            $ as
