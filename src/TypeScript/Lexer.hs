module TypeScript.Lexer where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec)
import TypeScript.Tokens (TsToken)

type Lexer = Parsec Void Text

lexTs :: Lexer [TsToken]
lexTs = undefined