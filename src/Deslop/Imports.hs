module Deslop.Imports where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Reader.Static
import Effectful.Reader.Static (ask)
import Effects.FileSystem
import TypeScript.AST
import TypeScript.Config

importAliases :: (Reader TsConfig :> es, FileSystem :> es) => TsProgram -> Eff es TsProgram
importAliases prog = do
    pure $ prog {ast = fixImport <$> prog.ast}
  where
    fixImport og@(Import _ t _) = og
    fixImport n = n

    fixTarget :: (FileSystem :> es, Reader TsConfig :> es) => Text -> Eff es Text
    fixTarget t = do
        fp <- T.pack <$> fullPath (T.unpack t)
        cfg <- ask @TsConfig
        pure $ fpWithAlias cfg fp

    fpWithAlias cfg fp =
        fromMaybe fp $
            (\(ImportAlias l p) -> T.replace p l fp)
                <$> find ((`T.isInfixOf` fp) . (.path)) cfg.paths