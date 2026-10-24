{- | Nix source to read: fragments that say which path literals they hold, and
text made of the characters a Nix reader has to be careful with.
-}
module Generators.Frontend.Nix.Source (
    Fragment (..),
    genSource,
    genPath,
    genTricky,
) where

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | A piece of Nix source, and the path literals a reader finds in it.
data Fragment = Fragment
    { text :: Text
    , paths :: [Text]
    }
    deriving stock (Show)

-- | Fragments of Nix source, and the source they spell, each followed by whitespace.
genSource :: Gen ([Fragment], Text)
genSource = do
    fragments <- Gen.list (Range.linear 0 12) genFragment
    separators <- Gen.list (Range.singleton (length fragments)) (Gen.element [" ", "\n", "\t "])
    pure (fragments, mconcat (zipWith (<>) (fmap (.text) fragments) separators))

-- | A path literal: relative, parent, absolute, home or bare, perhaps interpolated.
genPath :: Gen Text
genPath = do
    prefix <- Gen.element [".", "..", "", "~", "a"]
    segments <- Gen.list (Range.linear 1 3) genSegment
    pure (prefix <> foldMap ("/" <>) segments)
  where
    genSegment =
        Gen.choice
            [ Gen.text (Range.linear 1 6) (Gen.element ("abz09._-+" :: String))
            , (\a b -> a <> "${" <> b <> "}" <> ".nix") <$> Gen.text (Range.linear 0 3) Gen.alpha <*> Gen.element ["name", "x.y", "\"s\""]
            ]

-- | A path, or something a careless reader would take for one.
genFragment :: Gen Fragment
genFragment = do
    path <- genPath
    Gen.element
        [ Fragment path [path]
        , Fragment ("\"a${ " <> path <> " }b\"") [path]
        , Fragment ("''\n  ${ " <> path <> " }\n''") [path]
        , Fragment ("# " <> path <> "\n") []
        , Fragment ("/* " <> path <> " */") []
        , Fragment ("\"" <> path <> "\"") []
        , Fragment ("''" <> path <> "''") []
        , Fragment "a / b // c" []
        , Fragment "<nixpkgs/lib>" []
        , Fragment "https://example.org/a" []
        , Fragment "{ x = 1; }" []
        ]

{- | Text made only of what opens or closes a string, a comment, a path, an
interpolation or a search path, so that most of it is not Nix at all.
-}
genTricky :: Gen Text
genTricky = Gen.text (Range.linear 0 200) (Gen.element ("\"'$/{}#*.\n ab~<>:+\\" :: String))
