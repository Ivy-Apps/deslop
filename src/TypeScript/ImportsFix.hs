module TypeScript.ImportsFix where

import TypeScript.Tokens (TsProgram)

fixImports :: TsProgram -> TsProgram
fixImports = id