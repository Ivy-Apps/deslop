-- | Reading back what an explorer page carries.
module Fixtures.Deslop.Explore.Html (graphIn) where

import Data.Aeson (Value, decodeStrict)
import Data.Text qualified as T

-- | The graph a page carries inline, decoded as the page decodes it.
graphIn :: Text -> Maybe Value
graphIn page = do
    body <- T.stripPrefix opening . snd $ T.breakOn opening page
    decodeStrict . encodeUtf8 . fst $ T.breakOn "</script>" body
  where
    opening = "<script id=\"graph\" type=\"application/json\">"
