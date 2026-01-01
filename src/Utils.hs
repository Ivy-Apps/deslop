module Utils where

import Data.Text
import Data.Void
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safePop :: [a] -> [a]
safePop [] = []
safePop (_ : xs) = xs