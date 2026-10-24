module Frontend.Haskell.ModuleSpec (spec) where

import Data.Text qualified as T
import Deslop.Module (DependencyEdge (..), Location (..), Module (..), ModuleName (..), canonicalName)
import Fixtures.Frontend.Haskell.Project (Lowered (..), edgesFrom, lowerFiles, moduleNamed)
import FileSystem.Path (portablePath)
import Renderable qualified
import Test.Hspec
import TestUtils (requireJust, requireRight)

cabal :: ByteString
cabal =
    encodeUtf8 . T.unlines $
        [ "cabal-version: 3.0"
        , "name: p"
        , "version: 0"
        , "library"
        , "  hs-source-dirs: src"
        , "executable p"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: app"
        , "  build-depends: base, p"
        , "test-suite p-test"
        , "  type: exitcode-stdio-1.0"
        , "  main-is: Spec.hs"
        , "  hs-source-dirs: test"
        , "  build-depends: base, p"
        , "benchmark p-bench"
        , "  type: exitcode-stdio-1.0"
        , "  main-is: Bench.hs"
        , "  hs-source-dirs: bench"
        , "  build-depends: base"
        ]

files :: [(Text, ByteString)]
files =
    [ ("p.cabal", cabal)
    ,
        ( "src/P/A.hs"
        , "module P.A (module P.B, module X, module P.A, module Data.Text) where\n\
          \import P.B\n\
          \import qualified P.C as X\n\
          \import Data.Text (Text)\n\
          \import \"text\" Data.Text.IO\n\
          \import Paths_p\n\
          \import Helper\n"
        )
    , ("src/P/B.hs", "module P.B where\n")
    , ("src/P/C.hs", "module P.C where\n")
    , ("src/Utils.hs", "module Utils where\n")
    , ("src/P/Wrong.hs", "module P.Right where\n")
    , ("src/P/Cpp.hs", "{-# LANGUAGE CPP #-}\nmodule P.Cpp where\nimport P.B\n")
    , ("app/Main.hs", "import P.A\nimport Helper\nimport \"p\" P.B\nimport \"this\" P.C\nmain = pure ()\n")
    , ("app/Helper.hs", "module Helper where\n")
    , ("test/Spec.hs", "-- | The driver.\n{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n")
    , ("test/P/ASpec.hs", "module P.ASpec where\nimport P.A\nimport Utils\n")
    , ("test/Utils.hs", "module Utils where\n")
    , ("bench/Bench.hs", "import P.A\nmain = pure ()\n")
    ]

spec :: Spec
spec = describe "Frontend.Haskell.Module" $ do
    lowered <- runIO $ requireRight (toString . Renderable.render) =<< lowerFiles files
    let namesOf name = maybe [] (fmap (.text) . toList . (.names)) (moduleNamed name lowered)

    describe "naming" $ do
        it "lowers every file to one module" $
            length lowered.modules `shouldBe` length files - 1

        it "names a module by its path, aliased by its component" $
            namesOf "P/A" `shouldBe` ["P/A", "lib/p/P/A"]

        it "names an executable's main-is by its component, aliased Main" $
            namesOf "exe/p/Main" `shouldBe` ["exe/p/Main", "Main"]

        it "names a test suite's main-is by its component, aliased Main" $
            namesOf "test/p-test/Main" `shouldBe` ["test/p-test/Main", "Main"]

        it "names modules of two components sharing a name each by its component" $
            (namesOf "lib/p/Utils", namesOf "test/p-test/Utils")
                `shouldBe` (["lib/p/Utils", "Utils"], ["test/p-test/Utils", "Utils"])

        it "gives every module a canonical name no other shares" $
            length (ordNub (canonicalName <$> lowered.modules)) `shouldBe` length lowered.modules

        it "names a module by its path when its header says otherwise" $
            namesOf "P/Wrong" `shouldBe` ["P/Wrong", "lib/p/P/Wrong"]

    describe "imports" $ do
        it "resolve to the importing component's modules, and others are external" $
            edgesFrom "P/A" lowered
                `shouldBe` [ ("P.B", "P/B", "ImportEdge")
                           , ("P.C", "P/C", "ImportEdge")
                           , ("Data.Text", "External:Data/Text", "ImportEdge")
                           , ("Data.Text.IO", "External:Data/Text/IO", "ImportEdge")
                           , ("Paths_p", "External:Paths_p", "ImportEdge")
                           , -- The library sees only its own modules.
                             ("Helper", "External:Helper", "ImportEdge")
                           , ("P.B", "P/B", "ReExportEdge")
                           , ("X", "P/C", "ReExportEdge")
                           , ("Data.Text", "External:Data/Text", "ReExportEdge")
                           ]

        it "resolve to the library a component depends on, and to its own modules" $
            edgesFrom "exe/p/Main" lowered
                `shouldBe` [ ("P.A", "P/A", "ImportEdge")
                           , ("Helper", "Helper", "ImportEdge")
                           , ("P.B", "P/B", "ImportEdge")
                           , -- "this" is the importing component only.
                             ("P.C", "External:P/C", "ImportEdge")
                           ]

        it "resolve to the component's own module before the library's of the same name" $
            edgesFrom "P/ASpec" lowered
                `shouldBe` [("P.A", "P/A", "ImportEdge"), ("Utils", "test/p-test/Utils", "ImportEdge")]

        it "are external for a component that does not depend on the library" $
            edgesFrom "bench/p-bench/Main" lowered `shouldBe` [("P.A", "External:P/A", "ImportEdge")]

        it "are quoted where they are written" $ do
            m <- requireJust "P/A" (moduleNamed "P/A" lowered)

            fmap (\e -> (portablePath e.location.file, e.location.line, e.location.code)) (take 2 m.edges)
                `shouldBe` [("src/P/A.hs", 2, "import P.B"), ("src/P/A.hs", 3, "import qualified P.C as X")]

        it "quote a re-export at its export item" $ do
            m <- requireJust "P/A" (moduleNamed "P/A" lowered)

            fmap (\e -> (e.location.line, e.location.code)) (drop 6 m.edges)
                `shouldBe` [(1, "module P.B"), (1, "module X"), (1, "module Data.Text")]

    describe "an hspec-discover driver" $ do
        it "imports every spec below its directory" $
            edgesFrom "test/p-test/Main" lowered `shouldBe` [("P.ASpec", "P/ASpec", "ImportEdge")]

        it "is quoted at its pragma" $ do
            m <- requireJust "the driver" (moduleNamed "test/p-test/Main" lowered)

            fmap (\e -> (e.location.line, e.location.code)) m.edges
                `shouldBe` [(2, "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}")]

    describe "a file whose imports were not read" $ do
        it "is still a module, with no edges" $
            (namesOf "P/Cpp", edgesFrom "P/Cpp" lowered) `shouldBe` (["P/Cpp", "lib/p/P/Cpp"], [])

    describe "warnings" $
        it "name what was not read, what disagrees and what was renamed" $
            fmap Renderable.render lowered.warnings
                `shouldBe` [ "src/P/Cpp.hs is preprocessed by CPP (line 1), so its imports were not read."
                           , "src/P/Wrong.hs declares module P.Right, but GHC looks it up by its path as P.Wrong; Deslop names it P/Wrong."
                           , "src/Utils.hs test/Utils.hs are all called Utils, so each is named by its component instead."
                           ]
