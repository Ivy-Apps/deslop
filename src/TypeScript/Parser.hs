module TypeScript.Parser (
    parseTs,
) where

import Data.Bifunctor
import Data.Text
import Text.Megaparsec
import TypeScript.AST
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
        TsModule f.path . buildAST <$> runParser lexer f.path f.content

buildAST :: [TsToken] -> [TsNode]
buildAST = fmap node
  where
    node :: TsToken -> TsNode
    node (TsToken r RawK) = Source r
    node (TsToken r WhitespaceK) = Source r
    node (TsToken r (CommentK c)) = Comment {raw = r, content = c}
    node (TsToken r (DocsK c)) = Docs {raw = r, content = c}
    node (TsToken r ImportK) = undefined