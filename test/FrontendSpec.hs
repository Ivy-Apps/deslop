module FrontendSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..), SomeFrontend (..), Supported (..), detectFirst)
import Doubles.FileSystem (mockFiles, runReadOnly)
import Doubles.Frontend (LinesFrontend)
import FileSystem.Path (AbsPath (..))
import Fixtures.FileSystem.Path (inProject, projectRoot)
import Frontend (detectFrontend, supported)
import Frontend.TypeScript (TsFrontend)
import System.OsPath (OsPath)
import Test.Hspec

-- | Which language, by label, a project holding exactly these files is read as.
detectedIn :: [Supported] -> [OsPath] -> IO (Maybe Text)
detectedIn languages files =
    fmap labelOf <$> runReadOnly (mockFiles files) (detectFirst languages projectRoot)

labelOf :: SomeFrontend -> Text
labelOf (SomeFrontend fe) = label fe

bothMarkers :: [OsPath]
bothMarkers = [(inProject "tsconfig.json").osPath, (inProject "lines.project").osPath]

spec :: Spec
spec = describe "Frontend" $ do
    describe "the registry" $ do
        it "detects nothing in a project no language recognises" $ do
            detected <- runReadOnly (mockFiles [(inProject "package.json").osPath]) $ detectFrontend projectRoot

            fmap labelOf detected `shouldBe` Nothing

        it "detects nothing for a tsconfig.json that is not at the root" $ do
            detected <- runReadOnly (mockFiles [(inProject "app/tsconfig.json").osPath]) $ detectFrontend projectRoot

            fmap labelOf detected `shouldBe` Nothing

        it "tries TypeScript first, then Haskell" $
            fmap (\(Supported proxy) -> lookingFor proxy) supported `shouldBe` ["TypeScript (tsconfig.json)", "Haskell (*.cabal)"]

    describe "a polyglot project" $ do
        it "is read as the first language that recognises it" $ do
            detectedIn [Supported (Proxy @TsFrontend), Supported (Proxy @LinesFrontend)] bothMarkers
                `shouldReturn` Just "TypeScript"
            detectedIn [Supported (Proxy @LinesFrontend), Supported (Proxy @TsFrontend)] bothMarkers
                `shouldReturn` Just "Lines"

        it "falls through to a later language when an earlier one does not match" $
            detectedIn [Supported (Proxy @TsFrontend), Supported (Proxy @LinesFrontend)] [(inProject "lines.project").osPath]
                `shouldReturn` Just "Lines"

        it "detects nothing with no languages to try" $
            detectedIn [] bothMarkers `shouldReturn` Nothing
