{- | Turning a value back into the text it was read from.

The instances that matter are the round-tripping ones: a concrete syntax tree
renders to the exact bytes it was parsed from, which is what lets @deslop fix@
rewrite one import and leave every other character of the file alone.
-}
module Renderable (Renderable (..)) where

class Renderable a where
    render :: a -> Text

instance (Renderable a) => Renderable [a] where
    render = foldl' (\acc x -> acc <> render x) ""
