module Utils (
    Parser,
    todo,
    hush,
    safeHead,
    safePop,
    headErr,
    headOrThrow,
    dropCommonPre,
) where

import Data.Text qualified as T
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

todo :: a
todo = error "TODO"

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safePop :: [a] -> [a]
safePop [] = []
safePop (_ : xs) = xs

headErr :: e -> [a] -> Either e a
headErr e [] = Left e
headErr _ (x : _) = Right x

headOrThrow :: [a] -> a
headOrThrow [] = error "Expected the list to be non-empty"
headOrThrow (x : _) = x

dropCommonPre :: (Text, Text) -> (Text, Text)
dropCommonPre (x, y) = case T.commonPrefixes x y of
    Just (_, x', y') -> (x', y')
    Nothing -> (x, y)
