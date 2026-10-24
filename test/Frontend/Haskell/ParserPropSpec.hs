module Frontend.Haskell.ParserPropSpec (spec) where

import Fixtures.Frontend.Haskell.CST (headerNameOf, reExportsOf)
import Frontend.Haskell.CST (HsSource (..), Import (..), ImportDecl (..), ModuleExport (..), Qualification (..))
import Frontend.Haskell.Lexer (lexHaskell)
import Frontend.Haskell.ModuleName (dotted)
import Frontend.Haskell.Parser (parseHaskell)
import Generators.Frontend.Haskell.Source (ImportShape (..), Shape (..), genShape, spell)
import Hedgehog (annotate, evalMaybe, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Renderable qualified
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Frontend.Haskell.ParserProp" $ do
    prop "gives back every spelling of a module header it parses" $ do
        text <- forAll (genShape >>= spell)
        source <- evalMaybe (parseHaskell (lexHaskell text))
        Renderable.render source === text

    prop "finds exactly the imports and re-exports a header says, however it is spelled" $ do
        shape <- forAll genShape
        text <- forAll (spell shape)
        source <- evalMaybe (parseHaskell (lexHaskell text))
        annotate (show source)
        said source === (shape.header, shape.reExports, [(i.package, i.name, i.qualified, i.alias) | i <- shape.imports])

    prop "gives back any text, whether it parses or not" $ do
        text <- forAll $ Gen.text (Range.linear 0 200) (Gen.element ("module where import ( ) ; { } -- {- -} \n \" M" :: String))
        maybe (pure ()) ((=== text) . Renderable.render) (parseHaskell (lexHaskell text))
  where
    said source =
        ( headerNameOf source
        , dotted . (.name) <$> reExportsOf source
        , [(i.decl.package, dotted i.decl.moduleName, i.decl.qualification == Qualified, dotted <$> i.decl.alias) | i <- source.imports]
        )
