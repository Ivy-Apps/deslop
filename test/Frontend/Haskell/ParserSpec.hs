module Frontend.Haskell.ParserSpec (spec) where

import Fixtures.Frontend.Haskell.CST (headerNameOf, reExportsOf)
import Frontend.Haskell.CST (Header (..), HsSource (..), Import (..), ImportDecl (..), ModuleExport (..), Qualification (..))
import Frontend.Haskell.Lexer (lexHaskell)
import Frontend.Haskell.ModuleName (dotted)
import Frontend.Haskell.Parser (parseHaskell)
import Renderable qualified
import Test.Hspec
import TestUtils (requireJust)

parsed :: Text -> IO HsSource
parsed = requireJust "expected the source to parse" . parseHaskell . lexHaskell

-- | What each import reaches: package, module, alias, line.
importsOf :: Text -> IO [(Maybe Text, Text, Maybe Text, Int)]
importsOf = fmap (fmap summary . (.imports)) . parsed
  where
    summary i = (i.decl.package, dotted i.decl.moduleName, dotted <$> i.decl.alias, i.line)

modulesOf :: Text -> IO [Text]
modulesOf = fmap (fmap (\(_, m, _, _) -> m)) . importsOf

bodyOf :: Text -> IO Text
bodyOf = fmap (Renderable.render . (.body)) . parsed

spec :: Spec
spec = describe "Frontend.Haskell.Parser" $ do
    describe "the module header" $ do
        it "reads the module's name" $ do
            source <- parsed "module Data.Thing where\n"

            headerNameOf source `shouldBe` Just "Data.Thing"

        it "reads a file without one as Main" $ do
            source <- parsed "import A\nmain = pure ()\n"

            source.header `shouldBe` ImplicitMain

        it "reads a header across lines, with comments inside" $ do
            source <- parsed "module\n  -- the name\n  Data.Thing\n  {- exports -} (\n    x\n  ) where\nimport A\n"

            headerNameOf source `shouldBe` Just "Data.Thing"

        it "reads past a deprecation pragma" $ do
            source <- parsed "module M {-# DEPRECATED \"old\" #-} (x) where\n"

            headerNameOf source `shouldBe` Just "M"

        it "reads the module re-exports of its export list, and only those" $ do
            source <- parsed "module M (\n    module Data.Text,\n    x,\n    T (..),\n    module X,\n) where\n"

            fmap (\e -> (dotted e.name, e.line, e.code)) (reExportsOf source)
                `shouldBe` [("Data.Text", 2, "module Data.Text"), ("X", 5, "module X")]

        it "finds no re-exports in an item that only mentions a module" $ do
            source <- parsed "module M (pattern P, type (++), f) where\n"

            reExportsOf source `shouldBe` []

        it "does not take a where inside the export list for the header's" $ do
            source <- parsed "module M ((<>)) where\nimport A\n"

            headerNameOf source `shouldBe` Just "M"

        it "refuses a header that never reaches where" $
            parseHaskell (lexHaskell "module M (x)\nimport A\n") `shouldBe` Nothing

        it "refuses a header with something between the name and the export list" $
            parseHaskell (lexHaskell "module M x (y) where\n") `shouldBe` Nothing

    describe "import forms" $ do
        it "reads a plain import" $
            importsOf "module M where\nimport Data.Text\n" `shouldReturn` [(Nothing, "Data.Text", Nothing, 2)]

        it "reads qualified before the name, with an alias" $
            importsOf "import qualified Data.Map as M\n" `shouldReturn` [(Nothing, "Data.Map", Just "M", 1)]

        it "reads qualified after the name, with an alias" $
            importsOf "import Data.Map qualified as M\n" `shouldReturn` [(Nothing, "Data.Map", Just "M", 1)]

        it "reads an import list, nested parentheses and all" $
            modulesOf "import Data.Map (Map, (!), Foo (..), type (++), pattern P, data D)\nimport B\n"
                `shouldReturn` ["Data.Map", "B"]

        it "reads whether an import is qualified, before or after the name" $ do
            source <- parsed "import qualified A\nimport B qualified\nimport C as Q\n"

            fmap (.decl.qualification) source.imports `shouldBe` [Qualified, Qualified, Unqualified]

        it "reads hiding" $
            importsOf "import Prelude hiding (head)\n" `shouldReturn` [(Nothing, "Prelude", Nothing, 1)]

        it "reads a safe import" $
            modulesOf "import safe Data.Coerce\n" `shouldReturn` ["Data.Coerce"]

        it "reads a source import as the module it names" $
            modulesOf "import {-# SOURCE #-} A.B (x)\n" `shouldReturn` ["A.B"]

        it "reads a package import's package" $
            importsOf "import \"text\" Data.Text qualified as T\n" `shouldReturn` [(Just "text", "Data.Text", Just "T", 1)]

        it "reads every keyword at once, in GHC's order" $
            importsOf "import {-# SOURCE #-} safe qualified \"base\" Data.List as L hiding (x)\n"
                `shouldReturn` [(Just "base", "Data.List", Just "L", 1)]

        it "reads an import split across lines" $
            importsOf "import\n  qualified\n    Data.Map\n  as M\n  (\n    x\n  )\nimport B\n"
                `shouldReturn` [(Nothing, "Data.Map", Just "M", 1), (Nothing, "B", Nothing, 8)]

        it "refuses an import that names no module" $
            parseHaskell (lexHaskell "import qualified as M\n") `shouldBe` Nothing

        it "refuses an import with something after its import list" $
            parseHaskell (lexHaskell "import A (x) y\n") `shouldBe` Nothing

    describe "layout" $ do
        it "ends the import section at the first declaration that is not an import" $ do
            let source = "import A\nimport B\nx = 1\nimport C\n"

            modulesOf source `shouldReturn` ["A", "B"]
            bodyOf source `shouldReturn` "x = 1\nimport C\n"

        it "looks through comments between imports" $
            modulesOf "import A\n-- import B\n{- import C -}\nf :: Int\n" `shouldReturn` ["A"]

        it "reads imports indented to the layout column" $
            modulesOf "module M where\n  import A\n  import B\n  x = 1\n" `shouldReturn` ["A", "B"]

        it "reads a line indented past the layout column as the declaration above" $
            importsOf "module M where\n  import A\n    (x)\n  import B\n" `shouldReturn` [(Nothing, "A", Nothing, 2), (Nothing, "B", Nothing, 4)]

        it "reads imports separated by semicolons on one line" $
            modulesOf "module M where\nimport A; import B;; import C\n" `shouldReturn` ["A", "B", "C"]

        it "reads an explicitly braced module" $ do
            let source = "module M where {\nimport A\n; import B\n;x = 1 }\n"

            modulesOf source `shouldReturn` ["A", "B"]
            bodyOf source `shouldReturn` "x = 1 }\n"

        it "reads braced imports at any column" $
            modulesOf "module M where { import A;\nimport B\n    ; import C }" `shouldReturn` ["A", "B", "C"]

        it "reads a braced module with only imports" $
            bodyOf "module M where { import A; import B }" `shouldReturn` "}"

        it "reads a module of nothing but a header" $ do
            source <- parsed "module M where"

            (source.imports, source.body) `shouldBe` ([], [])

        it "reads an empty file" $ do
            source <- parsed ""

            (source.header, source.imports) `shouldBe` (ImplicitMain, [])

        it "keeps the leading pragmas and comments ahead of the header" $ do
            source <- parsed "{-# LANGUAGE CPP #-}\n-- | Docs\nmodule M where\n"

            Renderable.render source.leading `shouldBe` "{-# LANGUAGE CPP #-}\n-- | Docs\n"

        it "reads a byte-order mark ahead of the header" $
            fmap headerNameOf (parsed "\xFEFFmodule M where\nimport A\n") `shouldReturn` Just "M"

    describe "quoting" $ do
        it "quotes an import as written, without the trivia after it" $ do
            source <- parsed "import  qualified A as B -- why\n\nx = 1\n"

            fmap (Renderable.render . (.own)) source.imports `shouldBe` ["import  qualified A as B"]

    it "gives back the text it parsed" $ do
        let text = "{-# LANGUAGE X #-}\nmodule M (module A) where {\nimport A ; import B\n; x = 1 }\n"
        source <- parsed text

        Renderable.render source `shouldBe` text

