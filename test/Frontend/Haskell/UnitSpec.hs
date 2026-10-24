module Frontend.Haskell.UnitSpec (spec) where

import Fixtures.Frontend.Haskell.Project (cabalPackage, configFrom, inProject)
import Frontend.Haskell.CST (HsProgram (..), HsSource (..), Import (..), ImportDecl (..))
import Frontend.Haskell.Cabal (Component, HsConfig (..))
import Frontend.Haskell.ModuleName (fromDotted)
import Frontend.Haskell.Preprocessor (Preprocessor (..), Request (..), RequestedBy (..))
import Frontend.Haskell.Unit (HsUnit (..), Opacity (..), readUnit, renderUnit, unitPath)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop, requireJust)

configWith :: [Text] -> IO [Component]
configWith libraryFields =
    (.components) <$> configFrom (cabalPackage (["library", "  hs-source-dirs: src"] <> fmap ("  " <>) libraryFields))

-- | Why the file at @src/A.hs@ holding these bytes was not read, if it was not.
opacityOf :: [Component] -> ByteString -> Maybe Opacity
opacityOf components bytes = case readUnit components (inProject "src/A.hs") bytes of
    Readable _ -> Nothing
    Opaque _ why _ -> Just why

spec :: Spec
spec = describe "Frontend.Haskell.Unit" $ do
    plain <- runIO (configWith [])
    moduleB <- runIO . requireJust "a module name" $ fromDotted "B"

    describe "a file it reads" $ do
        it "keeps its path and its imports" $
            case readUnit plain (inProject "src/A.hs") "module A where\nimport B\n" of
                Readable program -> (program.path, fmap (.decl.moduleName) program.source.imports) `shouldBe` (inProject "src/A.hs", [moduleB])
                Opaque {} -> expectationFailure "expected the file to be read"

        it "is read through hspec-discover, whose output Deslop knows" $
            opacityOf plain "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n" `shouldBe` Nothing

    describe "a file it cannot read" $ do
        it "is one whose bytes are not UTF-8" $
            opacityOf plain "module A where\n\xff" `shouldBe` Just NotUtf8

        it "is one whose header never reaches where" $
            opacityOf plain "module A\nimport B\n" `shouldBe` Just Unparseable

        it "is one that asks for CPP in its pragmas" $
            opacityOf plain "{-# LANGUAGE CPP #-}\nmodule A where\n#if X\nimport B\n#endif\n"
                `shouldBe` Just (Preprocessed (Request Cpp (ByPragma 1 "{-# LANGUAGE CPP #-}")))

        -- Read through CPP before anything else: what CPP reads is not Haskell.
        it "is one using CPP even when the rest would not parse" $
            opacityOf plain "{-# LANGUAGE CPP #-}\nmodule A\n#if X\n where\n#endif\n"
                `shouldBe` Just (Preprocessed (Request Cpp (ByPragma 1 "{-# LANGUAGE CPP #-}")))

        it "is one whose component enables CPP for all its files" $ do
            components <- configWith ["default-extensions: CPP"]

            opacityOf components "module A where\n" `shouldBe` Just (Preprocessed (Request Cpp (ByComponent "lib:p")))

        it "is one that asks for any other -pgmF preprocessor" $
            opacityOf plain "{-# OPTIONS_GHC -F -pgmF my-pp #-}\nmodule A where\n"
                `shouldBe` Just (Preprocessed (Request (Custom "my-pp") (ByPragma 1 "{-# OPTIONS_GHC -F -pgmF my-pp #-}")))

    describe "rendering" $ do
        prop "gives back any bytes" $ do
            bytes <- forAll $ Gen.bytes (Range.linear 0 200)
            renderUnit (readUnit plain (inProject "src/A.hs") bytes) === bytes

        prop "gives back any text" $ do
            text <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
            renderUnit (readUnit plain (inProject "src/A.hs") (encodeUtf8 text)) === encodeUtf8 text

        it "keeps the path of a file it could not read" $
            unitPath (readUnit plain (inProject "src/A.hs") "\xff") `shouldBe` inProject "src/A.hs"
