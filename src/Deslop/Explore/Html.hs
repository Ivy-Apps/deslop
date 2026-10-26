{-# LANGUAGE TemplateHaskell #-}

{- | The module graph explorer: one HTML page that works opened straight from
disk, offline, with nothing beside it.

Everything it needs is inlined - the page from
@explorer\/src\/explorer.html@ and its scripts beside it, the vendored cytoscape and ELK libraries
(versions and licenses in @explorer\/vendor\/README.md@), and the 'Diagram' as
JSON from "Deslop.Explore.ModuleGraph.Json" - because a browser refuses a page opened from @file:\/\/@ any fetch of a
file next to it.

Every page it writes redistributes those libraries, so each carries the notice
its licence requires.
-}
module Deslop.Explore.Html (explorerPage) where

import Data.Aeson (encode)
import Data.Text qualified as T
import Deslop.Explore.Html.Template (Slot (..), Template, fillTemplate, parseTemplate)
import Deslop.Explore.ModuleGraph (Diagram)
import Deslop.Explore.ModuleGraph.Json (graphJson)
import FileSystem.Embed (embedParsed, embedText)

explorerPage :: Diagram -> Text
explorerPage d = fillTemplate content template
  where
    content = \case
        Graph -> scriptSafe . decodeUtf8 . encode $ graphJson d
        Scripts -> mconcat (libraries <> pageScripts)

template :: Template
template = $$(embedParsed parseTemplate "explorer/src/explorer.html")

-- | In load order: cytoscape-elk registers itself against the other two.
libraries :: [Text]
libraries =
    -- Its MIT notice is already inline, at the top of the file.
    [ script $$(embedText "explorer/vendor/cytoscape.min.js")
    , htmlComment elkjsNotice <> script $$(embedText "explorer/vendor/elk.bundled.js")
    , htmlComment cytoscapeElkNotice <> script $$(embedText "explorer/vendor/cytoscape-elk.js")
    ]

{- | The page's own scripts, in load order, after the libraries. Each but the
last defines one namespace and touches no DOM; @explorer.js@ wires them to the
page.
-}
pageScripts :: [Text]
pageScripts =
    script
        <$> [ $$(embedText "explorer/src/graph.js")
            , $$(embedText "explorer/src/colour.js")
            , $$(embedText "explorer/src/view.js")
            , $$(embedText "explorer/src/sha256.js")
            , $$(embedText "explorer/src/search.js")
            , $$(embedText "explorer/src/navigation.js")
            , $$(embedText "explorer/src/explorer.js")
            ]

script :: Text -> Text
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

{- | JSON that cannot end the script element holding it: a module named
@</script>@ would otherwise close it. @\\u003c@ is the same character to a
JSON parser.
-}
scriptSafe :: Text -> Text
scriptSafe = T.replace "<" "\\u003c"
