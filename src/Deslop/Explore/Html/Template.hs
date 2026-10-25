{-# LANGUAGE DeriveLift #-}

{- | The explorer page's template, split at its markers when Deslop is compiled.

The template marks each place the written page fills in with that 'Slot''s
marker. A template missing a marker, or carrying one twice, does not parse, so
the splice embedding it fails the build instead of shipping a page without its
graph or its libraries.

Its own module because a splice can only run what another module defines.
-}
module Deslop.Explore.Html.Template (
    Slot (..),
    Template,
    marker,
    parseTemplate,
    fillTemplate,
) where

import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Lift)

-- | A place in the page that is filled in when the page is written.
data Slot
    = -- | The vendored libraries' scripts, before the page's own script, which uses them.
      Libraries
    | -- | The graph, inside its @application/json@ script element.
      Graph
    deriving stock (Show, Eq, Enum, Bounded, Lift)

-- | How the template spells a 'Slot'.
marker :: Slot -> Text
marker = \case
    Libraries -> "<!-- deslop:libraries -->"
    Graph -> "/* deslop:graph */"

-- | The template's text around its 'Slot's, each of them exactly once.
newtype Template = Template [Piece]
    deriving stock (Lift)

data Piece = Verbatim Text | Hole Slot
    deriving stock (Lift)

-- | Splits a template at its markers. Each 'Slot' must be marked exactly once.
parseTemplate :: Text -> Either String Template
parseTemplate page = Template <$> foldlM holeFor [Verbatim page] universe
  where
    holeFor pieces slot =
        let cut = concatMap (punch slot) pieces
         in case length [() | Hole s <- cut, s == slot] of
                1 -> Right cut
                0 -> Left $ "no " <> toString (marker slot) <> " marker"
                _ -> Left $ "more than one " <> toString (marker slot) <> " marker"

    punch slot = \case
        Verbatim t -> intersperse (Hole slot) . fmap Verbatim $ T.splitOn (marker slot) t
        hole -> [hole]

-- | The page, each 'Slot' filled with what it is given.
fillTemplate :: (Slot -> Text) -> Template -> Text
fillTemplate content (Template pieces) = foldMap piece pieces
  where
    piece = \case
        Verbatim t -> t
        Hole slot -> content slot
