module Frontend.HaskellSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Doubles.FileSystem (defaultMockRoFileSystem, newInMemoryFileSystem, runInMemoryFileSystem, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Fixtures.Frontend.Haskell.Project (inProject, lowerFiles, projectRoot)
import Frontend.Haskell (HsFrontend (..))
import Frontend.Haskell.Cabal (HsConfig)
import Generators.Frontend.Haskell.Source (genShape, spell)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Renderable qualified
import Test.Hspec
import TestUtils (prop, requireRight)

detectedIn :: [(Text, ByteString)] -> IO (Maybe HsFrontend)
detectedIn files = do
    fs <- newInMemoryFileSystem [(inProject path, bytes) | (path, bytes) <- files]
    runEff . runInMemoryFileSystem fs $ detect projectRoot

cabal :: ByteString
cabal = "cabal-version: 3.0\nname: p\nversion: 0\nlibrary\n  hs-source-dirs: src\n"

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

        it "does not recognise a project with two, which is more than one package" $
            detectedIn [("a.cabal", cabal), ("b.cabal", cabal)] `shouldReturn` Nothing

    describe "loading the project" $ do
        it "refuses a cabal.project of several packages, naming them" $
            configErrorOf [("p.cabal", cabal), ("cabal.project", "packages: . lib/\n")]
                `shouldReturn` "cabal.project names packages other than the one at the root (lib/). Deslop reads a single Haskell package; run it on each package's directory instead."

        it "refuses a .cabal file Cabal cannot read, naming it" $ do
            err <- configErrorOf [("p.cabal", "cabal-version: 3.0\nname: p\nversion: 0\nlibrary\n  build-depends: base >=\n")]

            toString err `shouldStartWith` "Failed to read p.cabal:\n"

    describe "parsing" $ do
        cfg <- runIO loadedConfig

        prop "renders a parsed file back to its bytes" $ do
            text <- forAll (genShape >>= spell)
            let bytes = encodeUtf8 text
            render HsFrontend (parse HsFrontend cfg (inProject "src/M.hs") bytes) === bytes

        -- Total and lossless over any bytes at all, not only those of Haskell.
        prop "renders any bytes back to themselves" $ do
            bytes <- forAll $ Gen.bytes (Range.linear 0 200)
            render HsFrontend (parse HsFrontend cfg (inProject "src/M.hs") bytes) === bytes

    describe "linting" $
        it "finds nothing, having no built-in rules" $ do
            cfg <- loadedConfig
            findings <- runEff . runMockRoFileSystem defaultMockRoFileSystem . runReader projectRoot $ lint HsFrontend cfg (parse HsFrontend cfg (inProject "src/M.hs") "import A\n")

            length findings `shouldBe` 0
  where
    loadedConfig :: IO HsConfig
    loadedConfig = do
        fs <- newInMemoryFileSystem [(inProject "p.cabal", cabal)]
        loaded <- runEff . runInMemoryFileSystem fs $ loadConfig HsFrontend projectRoot
        fst <$> requireRight (toString . Renderable.render) loaded
