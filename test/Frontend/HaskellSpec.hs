module Frontend.HaskellSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Doubles.FileSystem (defaultMockRoFileSystem, newInMemoryFileSystem, runInMemoryFileSystem, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Fixtures.Frontend.Haskell.Project (cabalPackage, configFrom, inProject, lowerFiles, projectRoot)
import Frontend.Haskell (HsFrontend (..))
import Generators.Frontend.Haskell.Source (genShape, spell)
import Hedgehog (forAll, (===))
import Renderable qualified
import Test.Hspec
import TestUtils (prop)

detectedIn :: [(Text, ByteString)] -> IO (Maybe HsFrontend)
detectedIn files = do
    fs <- newInMemoryFileSystem [(inProject path, bytes) | (path, bytes) <- files]
    runEff . runInMemoryFileSystem fs $ detect projectRoot

cabal :: ByteString
cabal = encodeUtf8 (cabalPackage ["library\n  hs-source-dirs: src"])

-- | Why loading these files fails, as the person running Deslop reads it.
configErrorOf :: [(Text, ByteString)] -> IO Text
configErrorOf files = either Renderable.render (const "loaded") <$> lowerFiles files

spec :: Spec
spec = describe "Frontend.Haskell" $ do
    describe "what it tells the core about itself" $ do
        it "is labelled Haskell" $
            label HsFrontend `shouldBe` "Haskell"

        it "fences quoted statements as haskell" $
            codeFence HsFrontend `shouldBe` CodeFence "haskell"

        it "names a .cabal file as what it looks for" $
            lookingFor (Proxy @HsFrontend) `shouldBe` "Haskell (*.cabal)"

        it "brings no built-in rules" $
            builtInRules HsFrontend `shouldBe` []

    describe "detection" $ do
        it "recognises a project with one .cabal file at its root" $
            detectedIn [("p.cabal", cabal)] `shouldReturn` Just HsFrontend

        it "recognises a .cabal file it cannot parse, which loading then reports" $
            detectedIn [("p.cabal", "not: [a cabal file")] `shouldReturn` Just HsFrontend

        it "does not recognise a project without one" $
            detectedIn [("Main.hs", "main = pure ()")] `shouldReturn` Nothing

        it "does not recognise a .cabal file below the root" $
            detectedIn [("packages/p/p.cabal", cabal)] `shouldReturn` Nothing

        -- Loading then says why two packages are not read.
        it "recognises a project with two, which loading then refuses" $
            detectedIn [("a.cabal", cabal), ("b.cabal", cabal)] `shouldReturn` Just HsFrontend

        it "refuses two .cabal files at the root, naming them" $
            configErrorOf [("a.cabal", cabal), ("b.cabal", cabal)]
                `shouldReturn` "Deslop reads a single Haskell package, with one .cabal file at the project root, but found 2: a.cabal, b.cabal."

    describe "loading the project" $ do
        it "refuses a cabal.project of several packages, naming them" $
            configErrorOf [("p.cabal", cabal), ("cabal.project", "packages: . lib/\n")]
                `shouldReturn` "cabal.project names packages other than the one at the root (lib/). Deslop reads a single Haskell package; run it on each package's directory instead."

        it "refuses a .cabal file Cabal cannot read, naming it" $ do
            err <- configErrorOf [("p.cabal", "cabal-version: 3.0\nname: p\nversion: 0\nlibrary\n  build-depends: base >=\n")]

            toString err `shouldStartWith` "Failed to read p.cabal:\n"

    describe "parsing" $ do
        cfg <- runIO (configFrom (decodeUtf8 cabal))

        prop "renders a parsed file back to its bytes" $ do
            text <- forAll (genShape >>= spell)
            let bytes = encodeUtf8 text
            render HsFrontend (parse HsFrontend cfg (inProject "src/M.hs") bytes) === bytes

    describe "linting" $
        it "finds nothing, having no built-in rules" $ do
            cfg <- configFrom (decodeUtf8 cabal)
            findings <- runEff . runMockRoFileSystem defaultMockRoFileSystem . runReader projectRoot $ lint HsFrontend cfg (parse HsFrontend cfg (inProject "src/M.hs") "import A\n")

            length findings `shouldBe` 0
