module Frontend.Haskell.ModuleSpec (spec) where

import Deslop.Module (DependencyEdge (..), Location (..), Module (..), ModuleName (..), canonicalName)
import FileSystem.Path (portablePath)
import Fixtures.Frontend.Haskell.Project (Lowered (..), cabalPackage, edgesFrom, lowerFiles, moduleNamed)
import Renderable qualified
import Test.Hspec
import TestUtils (requireJust, requireRight)

cabal :: ByteString
cabal =
    encodeUtf8 . cabalPackage $
        [ "library"
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
          \import Helper\n\
          \import Utils as P.A\n"
        )
    , ("src/P/B.hs", "module P.B where\n")
    , ("src/P/C.hs", "module P.C where\n")
    , ("src/Utils.hs", "module Utils where\n")
    , ("src/P/Wrong.hs", "module P.Right where\n")
    , ("src/P/Cpp.hs", "{-# LANGUAGE CPP #-}\nmodule P.Cpp where\nimport P.B\n")
    , ("app/Main.hs", "import P.A\nimport Helper\nimport \"p\" P.B\nimport \"this\" P.C\nmain = pure ()\n")
    , ("app/Helper.hs", "module Helper where\n")
    , ("test/Spec.hs", "-- | The driver.\n{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n")
    , ("test/P/ASpec.hs", "module P.ASpec where\nimport P.A\nimport Utils\nimport \"p\" Utils\n")
    , ("test/Utils.hs", "module Utils where\n")
    , ("bench/Bench.hs", "import P.A\nmain = pure ()\n")
    ]

spec :: Spec
spec = describe "Frontend.Haskell.Module" $ do
    lowered <- runIO $ requireRight (toString . Renderable.render) =<< lowerFiles files
    let namesOf name = maybe [] (fmap (.text) . toList . (.names)) (moduleNamed name lowered)

    describe "naming" $ do
        it "lowers every source file to one module" $
            sort (fmap ((.text) . canonicalName) lowered.modules)
                `shouldBe` sort
                    [ "lib/p/P/A"
                    , "lib/p/P/B"
                    , "lib/p/P/C"
                    , "lib/p/Utils"
                    , "lib/p/P/Wrong"
                    , "lib/p/P/Cpp"
                    , "exe/p/Main"
                    , "exe/p/Helper"
                    , "test/p-test/Main"
                    , "test/p-test/P/ASpec"
                    , "test/p-test/Utils"
                    , "bench/p-bench/Main"
                    ]

        it "names a module by its component and its path, aliased by its path alone" $
            namesOf "lib/p/P/A" `shouldBe` ["lib/p/P/A", "P/A"]

        it "names an executable's main-is by its component, aliased Main" $
            namesOf "exe/p/Main" `shouldBe` ["exe/p/Main", "Main"]

        it "names a test suite's main-is by its component, aliased Main" $
            namesOf "test/p-test/Main" `shouldBe` ["test/p-test/Main", "Main"]

        it "names modules of two components sharing a bare name apart, without a warning" $
            (namesOf "lib/p/Utils", namesOf "test/p-test/Utils")
                `shouldBe` (["lib/p/Utils", "Utils"], ["test/p-test/Utils", "Utils"])

        it "gives every module a canonical name no other shares" $
            length (ordNub (canonicalName <$> lowered.modules)) `shouldBe` length lowered.modules

        it "names a module by its path when its header says otherwise" $
            namesOf "lib/p/P/Wrong" `shouldBe` ["lib/p/P/Wrong", "P/Wrong"]

    describe "imports" $ do
        it "resolve to the importing component's modules, and others are external" $
            edgesFrom "lib/p/P/A" lowered
                `shouldBe` [ ("P.B", "lib/p/P/B", "ImportEdge")
                           , ("P.C", "lib/p/P/C", "ImportEdge")
                           , ("Data.Text", "External:Data/Text", "ImportEdge")
                           , ("Data.Text.IO", "External:Data/Text/IO", "ImportEdge")
                           , ("Paths_p", "External:Paths_p", "ImportEdge")
                           , -- The library sees only its own modules.
                             ("Helper", "External:Helper", "ImportEdge")
                           , ("Utils", "lib/p/Utils", "ImportEdge")
                           , ("P.B", "lib/p/P/B", "ReExportEdge")
                           , -- module X names only the qualified import of P.C,
                             -- which brings nothing unqualified into scope.
                             -- module P.A is the import aliased as P.A.
                             ("P.A", "lib/p/Utils", "ReExportEdge")
                           , ("Data.Text", "External:Data/Text", "ReExportEdge")
                           ]

        it "resolve to the library a component depends on, and to its own modules" $
            edgesFrom "exe/p/Main" lowered
                `shouldBe` [ ("P.A", "lib/p/P/A", "ImportEdge")
                           , ("Helper", "exe/p/Helper", "ImportEdge")
                           , ("P.B", "lib/p/P/B", "ImportEdge")
                           , -- "this" is the importing component only.
                             ("P.C", "External:P/C", "ImportEdge")
                           ]

        it "resolve to the component's own module before the library's of the same name" $
            edgesFrom "test/p-test/P/ASpec" lowered
                `shouldBe` [ ("P.A", "lib/p/P/A", "ImportEdge")
                           , ("Utils", "test/p-test/Utils", "ImportEdge")
                           , -- A package import of the package itself searches its libraries only.
                             ("Utils", "lib/p/Utils", "ImportEdge")
                           ]

        it "are external for a component that does not depend on the library" $
            edgesFrom "bench/p-bench/Main" lowered `shouldBe` [("P.A", "External:P/A", "ImportEdge")]

        it "are quoted where they are written" $ do
            m <- requireJust "P/A" (moduleNamed "lib/p/P/A" lowered)

            fmap (\e -> (portablePath e.location.file, e.location.line, e.location.code)) (take 2 m.edges)
                `shouldBe` [("src/P/A.hs", 2, "import P.B"), ("src/P/A.hs", 3, "import qualified P.C as X")]

        it "quote a re-export at its export item" $ do
            m <- requireJust "P/A" (moduleNamed "lib/p/P/A" lowered)

            fmap (\e -> (e.location.line, e.location.code)) (drop 7 m.edges)
                `shouldBe` [(1, "module P.B"), (1, "module P.A"), (1, "module Data.Text")]

    describe "an hspec-discover driver" $ do
        it "imports every spec below its directory" $
            edgesFrom "test/p-test/Main" lowered `shouldBe` [("P.ASpec", "test/p-test/P/ASpec", "ImportEdge")]

        it "is quoted at its pragma" $ do
            m <- requireJust "the driver" (moduleNamed "test/p-test/Main" lowered)

            fmap (\e -> (e.location.line, e.location.code)) m.edges
                `shouldBe` [(2, "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}")]

    describe "a file whose imports were not read" $ do
        it "is still a module, with no edges" $
            (namesOf "lib/p/P/Cpp", edgesFrom "lib/p/P/Cpp" lowered) `shouldBe` (["lib/p/P/Cpp", "P/Cpp"], [])

    describe "warnings" $ do
        it "name what was not read and what disagrees" $
            fmap Renderable.render lowered.warnings
                `shouldBe` [ "src/P/Cpp.hs is preprocessed by CPP (line 1), so its imports were not read."
                           , "src/P/Wrong.hs declares module P.Right, but GHC looks it up by its path as P.Wrong; Deslop names it P/Wrong."
                           ]

        it "name files of one component claiming one name" $ do
            twice <-
                requireRight (toString . Renderable.render)
                    =<< lowerFiles
                        [ ("p.cabal", encodeUtf8 (cabalPackage ["library\n  hs-source-dirs: src, more"]))
                        , ("src/A.hs", "module A where\n")
                        , ("more/A.hs", "module A where\n")
                        ]

            fmap Renderable.render twice.warnings
                `shouldBe` ["more/A.hs src/A.hs are all lib/p/A, which GHC cannot tell apart; rename or move all but one."]

    describe "a source directory two components share" $ do
        shared <-
            runIO $
                requireRight (toString . Renderable.render)
                    =<< lowerFiles
                        [ ("p.cabal", encodeUtf8 (cabalPackage ["library\n  hs-source-dirs: src", "test-suite t\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: src, test"]))
                        , ("src/A.hs", "module A where\n")
                        , ("test/Main.hs", "import A\nmain = pure ()\n")
                        ]

        it "names its modules after their one owner" $
            fmap (fmap (.text) . toList . (.names)) (moduleNamed "lib/p/A" shared) `shouldBe` Just ["lib/p/A", "A"]

        -- GHC compiles src/A.hs into the test suite too, with no build-depends.
        it "lets every component compiling it import its modules" $
            edgesFrom "test/t/Main" shared `shouldBe` [("A", "lib/p/A", "ImportEdge")]

    describe "a component preprocessing every file through hspec-discover" $ do
        driven <-
            runIO $
                requireRight (toString . Renderable.render)
                    =<< lowerFiles
                        [ ("p.cabal", encodeUtf8 (cabalPackage ["test-suite t\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs: test\n  ghc-options: -F -pgmF hspec-discover"]))
                        , ("test/Spec.hs", "module Main where\n")
                        , ("test/ASpec.hs", "module ASpec where\n")
                        ]

        it "reads none of its files, and synthesises no imports" $
            fmap (.edges) driven.modules `shouldBe` [[], []]

        it "names each file in a warning" $
            fmap Renderable.render driven.warnings
                `shouldBe` [ "test/ASpec.hs is preprocessed by hspec-discover (enabled for test:t), so its imports were not read."
                           , "test/Spec.hs is preprocessed by hspec-discover (enabled for test:t), so its imports were not read."
                           ]
