module TypeScript.Lexer where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec)
import TypeScript.Tokens (TsToken (CommentTok, comment, raw))

type Lexer = Parsec Void Text

lexer :: Lexer [TsToken]
lexer =
  return
    [ CommentTok
        { comment = "hello"
        , raw = "// hello"
        }
    ]