module Deslop.Imports where

import TypeScript.AST (TsProgram)

fixImports :: TsProgram -> TsProgram
fixImports = id