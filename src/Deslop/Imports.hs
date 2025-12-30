module Deslop.Imports where

import Effectful
import Effectful.Reader.Static (Reader)
import TypeScript.AST (TsProgram)
import TypeScript.Config

fixImports :: (Reader TsConfig :> es) => TsProgram -> Eff es TsProgram
fixImports = pure . id