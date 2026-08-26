{- | Rulebook source text, for the specs that want to go in through the front
door and parse one.

A rulebook has a four-key envelope that no test is ever about, so 'rulebookYaml'
supplies it and takes only the lines of the single rule under test. Building the
YAML by hand rather than reaching for a @ToJSON@ keeps the specs honest: what is
asserted about is the text an author would have typed, which is the only thing
"Deslop.Rule.Book.Dto" ever sees.
-}
module Fixtures.Deslop.Rule.Book.Dto (
    rulebookYaml,
) where

import Data.Text qualified as T

-- | A one-rule rulebook whose target and clauses are supplied by the caller.
rulebookYaml :: [Text] -> ByteString
rulebookYaml ruleLines =
    encodeUtf8 . T.unlines $
        ["id: rb", "name: Rulebook", "description: d", "rules:", "  - id: a-rule", "    description: d"]
            <> ruleLines
            <> ["    fix: f"]
