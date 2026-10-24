{- | Generating Haskell module headers by generating what they say - a
'Shape' - and then spelling it out.

The point of the split is that one 'Shape' has many spellings: layout or
braces, comments and line breaks between any two tokens, @qualified@ before or
after the module name. A property can then say both "parsing finds what was
said" and "how it was said does not matter", which is the claim respelling
must not break.
-}
module Generators.Frontend.Haskell.Source (
    Shape (..),
    ImportShape (..),
    genShape,
    spell,
    projectModules,
    externalModules,
) where

import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | What a module says about its dependencies, however it is spelled.
data Shape = Shape
    { header :: Maybe Text
    -- ^ The module's name, or 'Nothing' for a file without a header.
    , reExports :: [Text]
    -- ^ The @module X@ items of its export list.
    , imports :: [ImportShape]
    }
    deriving stock (Show, Eq)

data ImportShape = ImportShape
    { package :: Maybe Text
    , name :: Text
    , alias :: Maybe Text
    , qualified :: Bool
    , source :: Bool
    , safe :: Bool
    , importList :: Maybe Text
    }
    deriving stock (Show, Eq)

-- | The modules the generated project holds: @src/A.hs@ and @src/B/C.hs@.
projectModules :: [Text]
projectModules = ["A", "B.C"]

externalModules :: [Text]
externalModules = ["Data.Text", "Data.Map.Strict", "Control.Monad"]

genShape :: Gen Shape
genShape = do
    header <- Gen.maybe (pure "M")
    imports <- Gen.list (Range.linear 0 6) genImport
    let scopeNames = [fromMaybe i.name i.alias | i <- imports]
    reExports <- case header of
        Nothing -> pure []
        Just self -> Gen.list (Range.linear 0 3) (Gen.element (self : "Absent" : scopeNames))
    pure Shape {header, reExports, imports}

genImport :: Gen ImportShape
genImport = do
    name <- Gen.element (projectModules <> externalModules)
    package <- Gen.frequency [(5, pure Nothing), (1, Just <$> Gen.element ["base", "containers"])]
    alias <- Gen.maybe (Gen.element ["T", "M", "X"])
    qualified <- Gen.bool
    source <- Gen.bool
    safe <- Gen.bool
    importList <- Gen.maybe (Gen.element ["()", "(x)", "(T (..), (<>))", "(type (++), pattern P)", "((!), f)"])
    pure ImportShape {package, name, alias, qualified, source, safe, importList}

{- | One spelling of a shape, chosen at random. Every spelling is a module GHC
would read the same way.
-}
spell :: Shape -> Gen Text
spell shape = do
    layout <- Gen.element [Column 1, Column 3, Braces]
    leading <- Gen.element ["", "-- | Docs\n", "{-# LANGUAGE ImportQualifiedPost #-}\n", "\xFEFF{- a {- nested -} comment -}\n"]
    header <- traverse (spellHeader shape.reExports) shape.header
    imports <- traverse (spellImport layout) shape.imports
    body <- Gen.element ["", "x = \"import Fake\"\n", "-- import Fake\nf :: Int\nf = 1\n", "data T = T\n"]
    pure . T.concat $ [leading, fromMaybe "" header, declarations layout (isJust shape.header) imports body]

data Layout = Column Int | Braces
    deriving stock (Eq)

-- | @module M (...) where@, before any layout begins, so any line breaks.
spellHeader :: [Text] -> Text -> Gen Text
spellHeader reExports name = do
    exports <- bool (Gen.maybe (spellExports reExports)) (Just <$> spellExports reExports) (not (null reExports))
    beforeName <- gap Braces
    beforeWhere <- gap Braces
    pure . T.concat $ ["module", beforeName, name, maybe "" (" " <>) exports, beforeWhere, "where"]

spellExports :: [Text] -> Gen Text
spellExports reExports = do
    let items = fmap ("module " <>) reExports <> ["x", "T (..)"]
    trailingComma <- Gen.element ["", ","]
    separators <- replicateM (length items) (Gen.element [", ", ",\n  ", "\n  , ", " {- c -}, "])
    pure $ "(" <> T.concat (zipWith (<>) ("" : separators) items) <> trailingComma <> ")"

spellImport :: Layout -> ImportShape -> Gen Text
spellImport layout i = do
    postQualified <- Gen.bool
    let words' =
            ["import"]
                <> ["{-# SOURCE #-}" | i.source]
                <> ["safe" | i.safe]
                <> ["qualified" | i.qualified && not postQualified]
                <> maybe [] (\p -> ["\"" <> p <> "\""]) i.package
                <> [i.name]
                <> ["qualified" | i.qualified && postQualified]
                <> maybe [] (\a -> ["as", a]) i.alias
                <> maybeToList i.importList
    gaps <- replicateM (length words' - 1) (gap layout)
    pure . T.concat $ zipWith (<>) words' (gaps <> [""])

{- | What may stand between two tokens of one declaration: spaces, comments,
and line breaks indented past the layout column.
-}
gap :: Layout -> Gen Text
gap layout =
    Gen.element
        [ " "
        , "  "
        , " {- c -} "
        , " {- a {- b -} c -} "
        , "\n" <> indent
        , " -- note\n" <> indent
        , "\n\n" <> indent
        ]
  where
    indent = T.replicate (column + 1) " "
    column = case layout of
        Column n -> n
        Braces -> 0

-- | The declarations after the header, delimited as the layout says.
declarations :: Layout -> Bool -> [Text] -> Text -> Text
declarations layout hasHeader imports body = case layout of
    Braces -> opening <> "{ " <> T.intercalate "\n; " (imports <> [body | not (T.null body)]) <> "\n}\n"
    Column n ->
        let indent = T.replicate (n - 1) " "
         in opening <> T.concat [indent <> d <> "\n" | d <- imports <> T.lines body]
  where
    opening = bool "" "\n" hasHeader
