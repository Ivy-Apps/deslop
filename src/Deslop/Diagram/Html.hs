{-# LANGUAGE TemplateHaskell #-}

{- | The module graph explorer: one HTML page that works opened straight from
disk, offline, with nothing beside it.

Everything it needs is inlined - the page and its script from
@assets\/diagram\/explorer.html@, the vendored cytoscape and ELK libraries
(versions and licenses in @assets\/diagram\/README.md@), and the 'Diagram' as
JSON - because a browser refuses a page opened from @file:\/\/@ any fetch of a
file next to it.
-}
module Deslop.Diagram.Html (
    explorerPage,
    librariesMarker,
    graphMarker,
) where

import Data.Aeson (encode)
import Data.Text qualified as T
import Deslop.Diagram (Diagram)
import FileSystem.Embed (embedText)

explorerPage :: Diagram -> Text
explorerPage d =
    fill
        template
        [ (graphMarker, scriptSafe . decodeUtf8 $ encode d)
        , (librariesMarker, foldMap script libraries)
        ]
  where
    script js = "<script>" <> js <> "</script>\n"

-- | Where the page wants the libraries: before its own script, which uses them.
librariesMarker :: Text
librariesMarker = "<!-- deslop:libraries -->"

-- | Where the page wants the graph: inside its @application/json@ script element.
graphMarker :: Text
graphMarker = "/* deslop:graph */"

template :: Text
template = $$(embedText "assets/diagram/explorer.html")

-- | In load order: cytoscape-elk registers itself against the other two.
libraries :: [Text]
libraries =
    [ $$(embedText "assets/diagram/cytoscape.min.js")
    , $$(embedText "assets/diagram/elk.bundled.js")
    , $$(embedText "assets/diagram/cytoscape-elk.js")
    ]

{- | Replaces each marker, in the order the page has them, with its content. What was inserted is never
searched again, so content that happens to spell a later marker stays as it is.
-}
fill :: Text -> [(Text, Text)] -> Text
fill page [] = page
fill page ((marker, content) : rest) = case T.breakOn marker page of
    (_, "") -> fill page rest
    (before, after) -> before <> content <> fill (T.drop (T.length marker) after) rest

{- | JSON that cannot end the script element holding it: a module named
@</script>@ would otherwise close it. @\\u003c@ is the same character to a
JSON parser.
-}
scriptSafe :: Text -> Text
scriptSafe = T.replace "<" "\\u003c"
