module Deslop.Imports where

import Effectful
import Effectful.Reader.Static (Reader)
import TypeScript.AST (TsProgram)
import TypeScript.Config
import Effects.FileSystem

fixImports :: (Reader TsConfig :> es, FileSystem :> es) => TsProgram -> Eff es TsProgram
fixImports = pure . id