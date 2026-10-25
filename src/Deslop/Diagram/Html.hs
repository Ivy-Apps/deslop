{-# LANGUAGE TemplateHaskell #-}

{- | The module graph explorer: one HTML page that works opened straight from
disk, offline, with nothing beside it.

Everything it needs is inlined - the page and its script from
@assets\/diagram\/explorer.html@, the vendored cytoscape and ELK libraries
(versions and licenses in @assets\/diagram\/README.md@), and the 'Diagram' as
JSON - because a browser refuses a page opened from @file:\/\/@ any fetch of a
file next to it.

Every page it writes redistributes those libraries, so each carries the notice
its licence requires.
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
        , (librariesMarker, mconcat libraries)
        ]

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
    -- Its MIT notice is already inline, at the top of the file.
    [ script $$(embedText "assets/diagram/cytoscape.min.js")
    , htmlComment elkjsNotice <> script $$(embedText "assets/diagram/elk.bundled.js")
    , htmlComment cytoscapeElkNotice <> script $$(embedText "assets/diagram/cytoscape-elk.js")
    ]
  where
    script js = "<script>" <> js <> "</script>\n"

{- | An HTML comment around any text: a @-->@ inside would end it early, so it
is spaced apart. Outside a script, a @</script>@ in it is inert.
-}
htmlComment :: Text -> Text
htmlComment t = "<!--\n" <> T.replace "-->" "- ->" t <> "-->\n"

-- | elkjs ships compiled, so the EPL-2.0 asks us to say where its source is.
elkjsNotice :: Text
elkjsNotice =
    unlines
        [ "elkjs 0.9.3, SPDX-License-Identifier: EPL-2.0"
        , "source: https://github.com/kieler/elkjs/tree/0.9.3"
        , "licence: https://www.eclipse.org/legal/epl-2.0/"
        ]

-- | The copyright and permission notice the MIT licence asks to travel with it.
cytoscapeElkNotice :: Text
cytoscapeElkNotice =
    unlines
        [ "cytoscape-elk 2.3.0, SPDX-License-Identifier: MIT"
        , "source: https://github.com/cytoscape/cytoscape.js-elk"
        , ""
        , "Copyright (c) 2016-2018, 2020, The Cytoscape Consortium."
        , ""
        , "Permission is hereby granted, free of charge, to any person obtaining a copy of"
        , "this software and associated documentation files (the \"Software\"), to deal in"
        , "the Software without restriction, including without limitation the rights to"
        , "use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies"
        , "of the Software, and to permit persons to whom the Software is furnished to do"
        , "so, subject to the following conditions:"
        , ""
        , "The above copyright notice and this permission notice shall be included in all"
        , "copies or substantial portions of the Software."
        , ""
        , "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
        , "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
        , "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
        , "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
        , "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,"
        , "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"
        , "SOFTWARE."
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
