module TypeScript.Parser where

import Data.Bifunctor
import Data.Text
import Data.Void
import Text.Megaparsec
import TypeScript.Lexer
import TypeScript.Tokens

data TsFile = TsFile
    { path :: FilePath
    , content :: Text
    }
    deriving (Show, Eq)

parseTs :: TsFile -> Either String TsProgram
parseTs f =
    first errorBundlePretty $
        TsModule f.path <$> runParser lexer f.path f.content