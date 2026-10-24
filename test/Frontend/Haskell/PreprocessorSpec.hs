module Frontend.Haskell.PreprocessorSpec (spec) where

import Frontend.Haskell.Lexer (lexHaskell)
import Frontend.Haskell.Preprocessor (
    Preprocessor (..),
    Request (..),
    RequestedBy (..),
    opaqueRequest,
    requestedByExtensions,
    requestedByOptions,
    requestedInFile,
 )
import Test.Hspec

inFile :: Text -> [Preprocessor]
inFile = fmap (.preprocessor) . requestedInFile . lexHaskell

spec :: Spec
spec = describe "Frontend.Haskell.Preprocessor" $ do
    describe "a file's own pragmas" $ do
        it "ask for CPP through LANGUAGE" $
            inFile "{-# LANGUAGE CPP #-}\nmodule M where" `shouldBe` [Cpp]

        it "ask for CPP among other extensions, in any case" $
            inFile "{-# language OverloadedStrings,CPP #-}" `shouldBe` [Cpp]

        it "ask for CPP through a GHC flag" $
            inFile "{-# OPTIONS_GHC -XCPP #-}\n{-# OPTIONS -cpp #-}" `shouldBe` [Cpp, Cpp]

        it "ask for hspec-discover through -F -pgmF" $
            inFile "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}" `shouldBe` [HspecDiscover]

        it "name hspec-discover by a path to it, too" $
            inFile "{-# OPTIONS_GHC -F -pgmF /nix/store/x/bin/hspec-discover -optF --no-main #-}" `shouldBe` [HspecDiscover]

        it "ask for any other -pgmF program by its name" $
            inFile "{-# OPTIONS_GHC -F -pgmFtasty-discover #-}" `shouldBe` [Custom "tasty-discover"]

        it "ask for nothing with -pgmF but no -F, which runs nothing" $
            inFile "{-# OPTIONS_GHC -pgmF hspec-discover #-}" `shouldBe` []

        it "are where they are and what they say" $
            requestedInFile (lexHaskell "-- | Docs\n\n{-# LANGUAGE CPP #-}\n")
                `shouldBe` [Request Cpp (ByPragma 3 "{-# LANGUAGE CPP #-}")]

        it "count only before the first token, where GHC reads them" $
            inFile "module M where\n{-# LANGUAGE CPP #-}\n" `shouldBe` []

        it "ask for nothing when they are other pragmas" $
            inFile "{-# LANGUAGE OverloadedStrings #-}\n{-# INLINE f #-}" `shouldBe` []

    describe "a component" $ do
        it "asks for CPP through its extensions" $
            requestedByExtensions "lib:p" ["CPP"] `shouldBe` [Request Cpp (ByComponent "lib:p")]

        it "asks for a preprocessor through its ghc-options" $
            requestedByOptions "exe:p" ["-Wall", "-F", "-pgmF", "my-pp"] `shouldBe` [Request (Custom "my-pp") (ByComponent "exe:p")]

    describe "what Deslop cannot read through" $ do
        it "is CPP" $
            opaqueRequest [Request Cpp (ByComponent "lib:p")] `shouldBe` Just (Request Cpp (ByComponent "lib:p"))

        it "is any -pgmF program but a file's own hspec-discover, whose output Deslop knows" $
            opaqueRequest [Request HspecDiscover (ByPragma 1 "p"), Request (Custom "pp") (ByComponent "c")]
                `shouldBe` Just (Request (Custom "pp") (ByComponent "c"))

        it "is nothing when a file's own pragma asks for hspec-discover" $
            opaqueRequest [Request HspecDiscover (ByPragma 1 "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}")] `shouldBe` Nothing

        -- Deslop knows what hspec-discover writes for one driver, not for
        -- every file of a component.
        it "is hspec-discover asked for by a component" $
            opaqueRequest [Request HspecDiscover (ByComponent "c")] `shouldBe` Just (Request HspecDiscover (ByComponent "c"))
