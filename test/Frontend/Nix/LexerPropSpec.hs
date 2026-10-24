module Frontend.Nix.LexerPropSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Frontend.Nix.Lexer (PathLiteralSpan (..), lexNix, pathLiteralSpans)
import Generators.Frontend.Nix.Source (Fragment (..), genPath, genSource, genTricky)
import Hedgehog (annotate, evalMaybe, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Renderable qualified
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Frontend.Nix.LexerProp" $ do
    prop "gives back any text it lexes" $ do
        text <- forAll genTricky
        maybe (pure ()) ((=== text) . Renderable.render) (lexNix text)

    prop "gives back Nix source" $ do
        (_, text) <- forAll genSource
        lexemes <- evalMaybe (lexNix text)
        Renderable.render lexemes === text

    prop "finds every path written as code, and none written in a comment or a string" $ do
        (fragments, text) <- forAll genSource
        spans <- evalMaybe (pathLiteralSpans text)
        fmap (.literal) spans === concatMap (.paths) fragments

    prop "rejects any source with a path ending in a slash, as Nix does" $ do
        (_, text) <- forAll genSource
        path <- forAll genPath
        rest <- forAll $ Gen.element ["", " x", ";", ")", "*c*/", "/c", "/ ./b"]
        lexNix (text <> path <> "/" <> rest) === Nothing

    prop "spans the bytes each path literal is written in" $ do
        (_, text) <- forAll genSource
        spans <- evalMaybe (pathLiteralSpans text)
        let bytes = encodeUtf8 text
        annotate (T.unpack text)
        [decodeUtf8 (BS.take (s.end - s.start) (BS.drop s.start bytes)) | s <- spans] === fmap (.literal) spans
