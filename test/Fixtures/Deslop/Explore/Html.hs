-- | Reading back what an explorer page carries.
module Fixtures.Deslop.Explore.Html (graphIn, titleIn) where

import Data.Aeson (Value, decodeStrict)
import Data.Text qualified as T

-- | The graph a page carries inline, decoded as the page decodes it.
graphIn :: Text -> Maybe Value
graphIn = decodeStrict . encodeUtf8 <=< elementText "<script id=\"graph\" type=\"application/json\">" "</script>"

-- | The raw text of a page's @title@ element, up to the first thing that closes it.
titleIn :: Text -> Maybe Text
titleIn = elementText "<title>" "</title>"

elementText :: Text -> Text -> Text -> Maybe Text
elementText opening closing page =
    fst . T.breakOn closing <$> (T.stripPrefix opening . snd $ T.breakOn opening page)
