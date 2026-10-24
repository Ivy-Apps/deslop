-- | Path patterns as "Frontend.Nix.Reference" spells them.
module Fixtures.Frontend.Nix.Reference (lit) where

import Frontend.Nix.Reference (Piece (..), Segment)

-- | A segment written out in full, with nothing computed in it.
lit :: Text -> Segment
lit = pure . Literal
