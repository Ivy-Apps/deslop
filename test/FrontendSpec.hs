module FrontendSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..), SomeFrontend (..), Supported (..), detectFirst)
import Doubles.FileSystem (MockRoFileSystem (..), mockDirs, mockFiles, runMockRoFileSystem)
import Doubles.Frontend (LinesFrontend)
import Effectful (runEff)
import FileSystem.Path (ProjectRoot (..))
import Frontend (detectFrontend, lookedFor, supported)
import Frontend.TypeScript (TsFrontend)
import System.OsPath (OsPath, osp)
import Test.Hspec
import TestUtils (ap)

projectRoot :: ProjectRoot
projectRoot = ProjectRoot (ap "/project")

-- | Which language, by label, a project holding exactly these files is read as.
detectedIn :: [Supported] -> [OsPath] -> IO (Maybe Text)
detectedIn languages files =
    fmap labelOf <$> runEff (runMockRoFileSystem (mockFiles files) (detectFirst languages projectRoot))

labelOf :: SomeFrontend -> Text
labelOf (SomeFrontend fe) = label fe

bothMarkers :: [OsPath]
bothMarkers = [[osp|/project/tsconfig.json|], [osp|/project/lines.project|]]

spec :: Spec
spec = describe "Frontend" $ do
    describe "the registry" $ do
        it "detects TypeScript when tsconfig.json is at the root" $ do
            detected <- runEff . runMockRoFileSystem (mockFiles [[osp|/project/tsconfig.json|]]) $ detectFrontend projectRoot

            fmap labelOf detected `shouldBe` Just "TypeScript"

        it "detects nothing in a project no language recognises" $ do
            detected <- runEff . runMockRoFileSystem (mockFiles [[osp|/project/package.json|]]) $ detectFrontend projectRoot

            fmap labelOf detected `shouldBe` Nothing

        it "detects nothing for a tsconfig.json that is not at the root" $ do
            detected <- runEff . runMockRoFileSystem (mockFiles [[osp|/project/app/tsconfig.json|]]) $ detectFrontend projectRoot

            fmap labelOf detected `shouldBe` Nothing

        it "detects Haskell when one .cabal file is at the root" $ do
            detected <- runEff . runMockRoFileSystem (mockDirs [(ap "/project", [ap "/project/p.cabal"])]) {mockFileExists = pure . (== ap "/project/p.cabal")} $ detectFrontend projectRoot

            fmap labelOf detected `shouldBe` Just "Haskell"

        it "tries TypeScript first, then Haskell" $
            fmap (\(Supported proxy) -> lookingFor proxy) supported `shouldBe` ["TypeScript (tsconfig.json)", "Haskell (*.cabal)"]

        it "names what every language looks for" $
            lookedFor `shouldBe` ["TypeScript (tsconfig.json)", "Haskell (*.cabal)"]

    describe "a polyglot project" $ do
        it "is read as the first language that recognises it" $ do
            detectedIn [Supported (Proxy @TsFrontend), Supported (Proxy @LinesFrontend)] bothMarkers
                `shouldReturn` Just "TypeScript"
            detectedIn [Supported (Proxy @LinesFrontend), Supported (Proxy @TsFrontend)] bothMarkers
                `shouldReturn` Just "Lines"

        it "falls through to a later language when an earlier one does not match" $
            detectedIn [Supported (Proxy @TsFrontend), Supported (Proxy @LinesFrontend)] [[osp|/project/lines.project|]]
                `shouldReturn` Just "Lines"

        it "detects nothing with no languages to try" $
            detectedIn [] bothMarkers `shouldReturn` Nothing
