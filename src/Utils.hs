module Utils (
    Parser,
    Validation (..),
    validate,
    invalid,
    todo,
    hush,
    safeHead,
    safePop,
    headErr,
    headOrThrow,
    dropCommonPre,
    firstJustM,
) where

import Data.Text qualified as T
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

{- | An 'Either' that accumulates rather than short-circuits.

The point is the 'Applicative': @f \<$\> a \<*\> b@ reports the failures of
both @a@ and @b@, where 'Either' would report only the first. Deliberately not
a 'Monad' - accumulating and sequencing are incompatible, and where a later
step genuinely depends on an earlier one the caller should say so by pattern
matching on 'validate' instead.
-}
newtype Validation e a = Validation {either' :: Either e a}
    deriving stock (Show, Eq, Functor)

instance (Semigroup e) => Applicative (Validation e) where
    pure = Validation . Right
    Validation (Left e1) <*> Validation (Left e2) = Validation (Left (e1 <> e2))
    Validation (Left e) <*> _ = Validation (Left e)
    _ <*> Validation (Left e) = Validation (Left e)
    Validation (Right f) <*> Validation (Right a) = Validation (Right (f a))

validate :: Either e a -> Validation e a
validate = Validation

invalid :: e -> Validation e a
invalid = Validation . Left


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

firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM action (x : xs) = action x >>= maybe (firstJustM action xs) (pure . Just)
