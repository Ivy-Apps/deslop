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

    fixTarget t =
        uncurry fpWithAlias . bimap T.pack (.paths)
            <$> ((,) <$> fullPath (T.unpack t) <*> ask @TsConfig)

    fpWithAlias fp =
        fromMaybe fp
            . fmap (\(ImportAlias l p) -> T.replace p l fp)
            . find ((`T.isInfixOf` fp) . (.path))