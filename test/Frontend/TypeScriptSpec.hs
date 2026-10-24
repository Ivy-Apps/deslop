{-# LANGUAGE QuasiQuotes #-}

module Frontend.TypeScriptSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Module (Module (..))
import Doubles.FileSystem (mockDirs, mockFiles, runMockRoFileSystem, runReadOnly)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static (runReader)
import FileSystem.Path (AbsPath (..), ProjectRoot (..))
import Fixtures.FileSystem.Path (inProject, projectRoot)
import Fixtures.Frontend.TypeScript.Config (emptyTsConfig)
import Frontend.TypeScript (TsFrontend (..))
import Generators.Frontend.TypeScript.CST (genTsProgram)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Renderable qualified
import System.OsPath (osp)
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Frontend.TypeScript" $ do
    describe "detection" $ do
        it "recognises a project with a tsconfig.json at its root" $
            runReadOnly (mockFiles [[osp|/project/tsconfig.json|]]) (detect projectRoot)
                `shouldReturn` Just TsFrontend

        it "does not recognise a project without one" $
            runReadOnly (mockFiles []) (detect @TsFrontend projectRoot)
                `shouldReturn` Nothing

        it "does not recognise a project whose tsconfig.json is a directory" $ do
            let tsconfig = inProject "tsconfig.json"

            detected <-
                runReadOnly (mockDirs [(projectRoot.path, [tsconfig]), (tsconfig, [])]) $
                    detect @TsFrontend projectRoot

            detected `shouldBe` Nothing

    describe "parsing" $ do
        -- What `deslop fix` relies on to leave every byte but a specifier alone.
        prop "renders a parsed file back to its bytes" $ do
            planted <- forAll genTsProgram
            let bytes = encodeUtf8 (Renderable.render planted)
            render TsFrontend (parse TsFrontend emptyTsConfig (inProject "a.ts") bytes) === bytes

        -- Total and lossless over any bytes at all, not only those of TypeScript.
        prop "renders any bytes back to themselves" $ do
            bytes <- forAll $ Gen.bytes (Range.linear 0 200)
            render TsFrontend (parse TsFrontend emptyTsConfig (inProject "a.ts") bytes) === bytes

    describe "a file that is not UTF-8" $ do
        let path = inProject "src/a.ts"
            lowered =
                runEff
                    . runConcurrent
                    . runMockRoFileSystem (mockFiles [path.osPath])
                    . runReader projectRoot
                    . lower TsFrontend emptyTsConfig
                    $ [parse TsFrontend emptyTsConfig path "import x from './b';\n\xff"]

        it "is still a module, with no edges" $ do
            (ms, _) <- lowered
            fmap (.edges) ms `shouldBe` [[]]

        it "is named in a warning that its imports were not read" $ do
            (_, warnings) <- lowered
            fmap Renderable.render warnings `shouldBe` ["src/a.ts is not UTF-8, so its imports were not read."]
