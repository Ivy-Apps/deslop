module Frontend.Haskell.CSTSpec (spec) where

import Frontend.Haskell.CST (Header (..), HsSource (..), Import (..), ImportDecl (..), ModuleHeader (..), Qualification (..))
import Frontend.Haskell.ModuleName (fromDotted)
import Frontend.Haskell.Tokens (HsToken (..), HsTokenKind (..))
import Renderable qualified
import Test.Hspec
import TestUtils (requireJust)

spec :: Spec
spec = describe "Frontend.Haskell.CST" $ do
    it "renders every part in file order" $ do
        name <- requireJust "a module name" (fromDotted "A")
        let header = Declared ModuleHeader {name, reExports = [], tokens = [tok "module A where"]}
            imp = Import {decl = ImportDecl Nothing name Unqualified Nothing, line = 3, own = [tok "import A"], trailing = [tok "\n"]}

        Renderable.render (source header [imp]) `shouldBe` "-- lead\nmodule A where\nimport A\nx = 1\n"
  where
    tok raw = HsToken raw OtherK
    source header imports =
        HsSource
            { leading = [tok "-- lead\n"]
            , header
            , opening = [tok "\n" | header /= ImplicitMain]
            , imports
            , body = [tok "x = 1\n"]
            }
