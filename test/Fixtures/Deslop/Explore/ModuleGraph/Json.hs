-- | Reading the fields of a graph the way the explorer page does.
module Fixtures.Deslop.Explore.ModuleGraph.Json (
    textField,
    moduleTags,
    moduleField,
) where

import Data.Aeson (FromJSON, Key, Value, withObject, (.:))
import Data.Aeson.Types (parseMaybe)

-- | A top-level text field of the graph, which may be null.
textField :: Key -> Value -> Maybe (Maybe Text)
textField key = parseMaybe (withObject "graph" (.: key))

-- | Each module's id and tags.
moduleTags :: Value -> Maybe [(Text, [Text])]
moduleTags = moduleField "tags"

-- | Each module's id and one of its fields.
moduleField :: (FromJSON a) => Key -> Value -> Maybe [(Text, a)]
moduleField key = parseMaybe (withObject "graph" $ \o -> catMaybes <$> (traverse node =<< o .: "nodes"))
  where
    node = withObject "node" $ \n ->
        n .: "kind" >>= \case
            ("module" :: Text) -> Just <$> ((,) <$> n .: "id" <*> n .: key)
            _ -> pure Nothing
