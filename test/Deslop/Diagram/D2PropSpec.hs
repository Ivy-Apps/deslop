module Deslop.Diagram.D2PropSpec (spec) where

import Data.Text qualified as T
import Deslop.Diagram.D2 (quote)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Diagram.D2 properties" $ do
    prop "a quoted segment reads back as the text it quoted" $ do
        segment <- forAll . Gen.text (Range.linear 0 20) $ Gen.frequency [(3, Gen.element @[] "\"\\./[]@-> _"), (7, Gen.unicode)]
        unquote (quote segment) === Just segment

-- | How D2 reads a double-quoted string: @\\@ escapes the next character.
unquote :: Text -> Maybe Text
unquote t = T.stripPrefix "\"" t >>= go []
  where
    go acc rest = case T.uncons rest of
        Just ('\\', more) -> T.uncons more >>= \(c, more') -> go (c : acc) more'
        Just ('"', "") -> Just . T.pack . reverse $ acc
        Just ('"', _trailing) -> Nothing
        Just (c, more) -> go (c : acc) more
        Nothing -> Nothing
