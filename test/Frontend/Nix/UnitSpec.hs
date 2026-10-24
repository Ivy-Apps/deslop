module Frontend.Nix.UnitSpec (spec) where

import Data.ByteString qualified as BS
import FileSystem.Path (AbsPath)
import Fixtures.FileSystem.Path (inProject)
import Frontend.Nix.Unit (NixUnit (..), Opacity (..), readUnit, renderUnit, unitPath)
import Generators.Frontend.Nix.Source (genTricky)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

opacityOf :: NixUnit -> Maybe Opacity
opacityOf = \case
    Readable _ -> Nothing
    Opaque _ why _ -> Just why

file :: AbsPath
file = inProject "a.nix"

spec :: Spec
spec = describe "Frontend.Nix.Unit" $ do
    for_
        [ ("reads a file that lexes", "{ imports = [ ./b.nix ]; }", Nothing)
        , ("keeps a file that is not UTF-8 opaque", "\xff\xfe", Just NotUtf8)
        , ("keeps a file Nix could not lex opaque", "\"unterminated", Just Unlexable)
        ]
        $ \(behaviour, bytes, opacity) ->
            it behaviour $ opacityOf (readUnit file bytes) `shouldBe` opacity

    it "remembers where it was read from" $
        unitPath (readUnit file "\xff") `shouldBe` file

    prop "renders any bytes back" $ do
        bytes <- forAll $ Gen.choice [Gen.bytes (Range.linear 0 100), encodeUtf8 <$> genTricky]
        renderUnit (readUnit file bytes) === bytes

    it "renders nothing as nothing" $
        renderUnit (readUnit file BS.empty) `shouldBe` BS.empty
