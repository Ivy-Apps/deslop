module Frontend.Haskell.ModulePropSpec (spec) where

import Data.Text qualified as T
import Fixtures.Frontend.Haskell.Project (Lowered (..), cabalPackage, edgesFrom, lowerFiles)
import Generators.Frontend.Haskell.Source (ImportShape (..), Shape (..), genShape, projectModules, spell)
import Hedgehog (PropertyT, annotate, evalEither, forAll, (===))
import Renderable qualified
import Test.Hspec
import TestUtils (prop)

-- | The edges of @src/M.hs@ holding this text, beside @src/A.hs@ and @src/B/C.hs@.
edgesOfM :: Text -> PropertyT IO [(Text, Text, Text)]
edgesOfM text = do
    lowered <- liftIO (lowerFiles files) >>= evalEither . first Renderable.render
    annotate (show lowered.warnings)
    pure (edgesFrom "lib/p/M" lowered)
  where
    files =
        ("p.cabal", encodeUtf8 (cabalPackage ["library\n  hs-source-dirs: src"]))
            : ("src/M.hs", encodeUtf8 text)
            : [("src/" <> T.replace "." "/" m <> ".hs", encodeUtf8 ("module " <> m <> " where\n")) | m <- projectModules]

spec :: Spec
spec = describe "Frontend.Haskell.ModuleProp" $ do
    -- Layout or braces, comments, line breaks, qualified before or after: none
    -- of it changes what a module depends on.
    prop "lowers every spelling of one module to the same edges" $ do
        shape <- forAll genShape
        spelling <- forAll (spell shape)
        respelling <- forAll (spell shape)
        edges <- edgesOfM spelling
        respelled <- edgesOfM respelling
        edges === respelled

    prop "lowers a module to an edge per import, then one per import each module export names" $ do
        shape <- forAll genShape
        text <- forAll (spell shape)
        edges <- edgesOfM text
        edges === expectedEdges shape

{- | What the shape says, resolved by hand: a project module is reached by its
own name unless a package import names another package. @module X@ exports
every unqualified import named @X@, the module's own name included.
-}
expectedEdges :: Shape -> [(Text, Text, Text)]
expectedEdges shape =
    [(i.name, targetOf i, "ImportEdge") | i <- shape.imports]
        <> [ (export, targetOf i, "ReExportEdge")
           | export <- shape.reExports
           , i <- shape.imports
           , -- A qualified import brings nothing unqualified into scope, so
           -- module X exports none of it (Haskell 2010, 5.2).
           not i.qualified
           , fromMaybe i.name i.alias == export
           ]
  where
    targetOf i
        | i.name `elem` projectModules && isNothing i.package = "lib/p/" <> T.replace "." "/" i.name
        | otherwise = "External:" <> T.replace "." "/" i.name
