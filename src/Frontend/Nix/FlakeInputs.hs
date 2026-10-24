{- | The inputs a @flake.nix@ declares, read from its lexemes: the attribute
names in @inputs = { ... }@, and the @\<name\>@ of every @inputs.\<name\>...@
binding beside it. Only these are flake inputs: an @inputs.foo@ naming
anything else is some other attribute set.
-}
module Frontend.Nix.FlakeInputs (declaredInputs) where

import Data.Set qualified as Set
import Frontend.Nix.Lexeme (Lexeme (..), Part (..), isTrivia, nesting)

-- | A lexeme that is not trivia, how deep in braces it is, and what came before it.
data Placed = Placed
    { depth :: Int
    , previous :: Maybe Lexeme
    , lexeme :: Lexeme
    }

declaredInputs :: [Lexeme] -> Set Text
declaredInputs = Set.fromList . go . placed
  where
    go = \case
        p : rest | isBinding 1 p && p.lexeme == Identifier "inputs" -> case rest of
            Placed {lexeme = Punctuation "."} : n : rest' -> maybeToList (attributeName n.lexeme) <> go rest'
            Placed {lexeme = Punctuation "="} : Placed {lexeme = Punctuation "{"} : rest' ->
                let (block, rest'') = span ((>= 2) . (.depth)) rest'
                 in mapMaybe (attributeName . (.lexeme)) (filter (isBinding 2) block) <> go rest''
            _ -> go rest
        _ : rest -> go rest
        [] -> []

    -- The first word of a binding, at the top of an attribute set this deep.
    isBinding d p = p.depth == d && p.previous `elem` [Just (Punctuation "{"), Just (Punctuation ";")]

attributeName :: Lexeme -> Maybe Text
attributeName = \case
    Identifier "inherit" -> Nothing
    Identifier name -> Just name
    Str _ [Chunk name] -> Just name
    _ -> Nothing

placed :: [Lexeme] -> [Placed]
placed = snd . mapAccumL place (0, Nothing) . filter (not . isTrivia)
  where
    place (d, previous) lexeme = ((d + nesting lexeme, Just lexeme), Placed d previous lexeme)
