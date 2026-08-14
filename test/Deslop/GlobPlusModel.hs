{- | A model of Glob+ target patterns, written independently of the production
compiler, so that properties compare two implementations rather than comparing
one implementation with itself.

The model is deliberately coarser than Glob+: every slot occupies whole path
segments. That is enough to reach the capture plumbing - which is where the
group numbering, the @**\/@ idiom and the casing agreement all live - while
keeping pattern and path in a correspondence a test can assert against. The
segments a @**@ stands for always carry a @.@, which no variable capture regex
accepts, so a greedy @**@ cannot steal the segment planted for a variable and
the intended parse is the only parse.
-}
module Deslop.GlobPlusModel (
    -- * The model
    Slot (..),
    Affix (..),
    renderPattern,
    plantPath,
    matchesModel,
    unaffix,

    -- * Generators
    genSlots,
    genPlanting,
    genPerturbed,
    genName,
    genLongName,
    genValue,
    genWord,
    genRendering,
    Planting (..),
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T
import Deslop.Casing (Casing (..), render)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------
-- The model
--------------------------------------------------------------------------------

-- | One piece of a target pattern, occupying whole path segments.
data Slot
    = -- | A fixed segment.
      SlotLiteral Text
    | -- | @*@: exactly one segment.
      SlotStar
    | -- | @**@: zero or more segments.
      SlotGlobStar
    | -- | A variable filling one segment, between an optional literal prefix
      -- and suffix - the @use{{FileName}}ViewModel@ idiom.
      SlotVar [Text] Casing Affix
    deriving (Show, Eq)

{- | The literals a variable sits between inside its segment. Both may be
empty, in which case the variable is the whole segment.
-}
data Affix = Affix
    { prefix :: Text
    , suffix :: Text
    }
    deriving (Show, Eq)

renderPattern :: [Slot] -> Text
renderPattern = T.intercalate "/" . fmap slotText
  where
    slotText (SlotLiteral t) = t
    slotText SlotStar = "*"
    slotText SlotGlobStar = "**"
    slotText (SlotVar name casing affix) =
        affix.prefix <> "{{" <> render casing name <> "}}" <> affix.suffix

{- | What each slot was filled with. Kept alongside the path so a property can
assert the compiler recovered exactly what the generator put there.
-}
newtype Planting = Planting {segments :: [(Slot, [Text])]}
    deriving (Show, Eq)

plantPath :: Planting -> Text
plantPath = T.intercalate "/" . concatMap snd . (.segments)

{- | Whether the model says this path matches, by walking segments directly.
Only the decision is modelled, never which parse won: POSIX longest-match is
the production engine's business, and a second implementation of it would
prove nothing.
-}
matchesModel :: [Slot] -> [Text] -> Bool
matchesModel [] segments = null segments
matchesModel (SlotLiteral t : rest) segments = case segments of
    (s : more) -> s == t && matchesModel rest more
    [] -> False
matchesModel (SlotStar : rest) segments = case segments of
    (_ : more) -> matchesModel rest more
    [] -> False
matchesModel (SlotVar _ casing affix : rest) segments = case segments of
    (s : more) -> maybe False (capturable casing) (unaffix affix s) && matchesModel rest more
    [] -> False
matchesModel (SlotGlobStar : rest) segments =
    any (\n -> matchesModel rest (drop n segments)) [atLeast .. length segments]
  where
    -- A trailing ** compiles to '/.*', which still needs its slash, so
    -- '@/lib/**' matches below @/lib and not @/lib itself. In the middle it is
    -- the '**/' idiom, an optional group, and may stand for nothing at all.
    atLeast = if null rest then 1 else 0

{- | Strips a variable's surrounding literals off its segment. Both are fixed
and anchored - the prefix at the very start, the suffix at the very end - so
there is only one way to take them off.
-}
unaffix :: Affix -> Text -> Maybe Text
unaffix affix segment =
    T.stripPrefix affix.prefix segment >>= T.stripSuffix affix.suffix

{- | Whether a segment is something the given casing's capture group accepts.
Mirrors 'Deslop.GlobPlus.captureRegex' as a predicate, which is the point: the
property fails if either drifts.
-}
capturable :: Casing -> Text -> Bool
capturable PascalCase = matchesClass isAsciiUpper isAlphaNum
capturable CamelCase = matchesClass isAsciiLower isAlphaNum
capturable KebabCase = matchesClass isKebabChar isKebabChar
capturable ConstantCase = matchesClass isConstantChar isConstantChar

matchesClass :: (Char -> Bool) -> (Char -> Bool) -> Text -> Bool
matchesClass isFirst isRest t = case T.uncons t of
    Just (c, rest) -> isFirst c && T.all isRest rest
    Nothing -> False

isAlphaNum, isKebabChar, isConstantChar :: Char -> Bool
isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c
isKebabChar c = isAsciiLower c || isDigit c || c == '-'
isConstantChar c = isAsciiUpper c || isDigit c || c == '_'

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

{- | A pattern of 2-5 slots. Variables get distinct names so that a binding can
be attributed, and no two globstars are adjacent - @**\/**@ is degenerate and
says nothing the single case does not.
-}
genSlots :: Gen [Slot]
genSlots = do
    count <- Gen.int (Range.linear 2 5)
    slots <- go count varNames
    pure (dropAdjacentGlobStars slots)
  where
    go 0 _ = pure []
    go n available = do
        slot <- genSlot available
        rest <- go (n - 1 :: Int) (drop (usedNames slot) available)
        pure (slot : rest)

    usedNames (SlotVar {}) = 1
    usedNames _ = 0

    genSlot available =
        Gen.choice $
            [ SlotLiteral <$> Gen.element ["app", "features", "components", "lib"]
            , pure SlotStar
            , pure SlotGlobStar
            ]
                <> [ SlotVar <$> genNamed stem <*> Gen.element allCasings <*> genAffix
                   | stem <- take 1 available
                   ]

    -- 2-4 words: a real variable is often three, as {{use-case-name}} is, and
    -- the extra words are where a Pascal spelling gains most of its acronym
    -- readings.
    genNamed stem = do
        leading <- Gen.list (Range.linear 1 3) genWord
        pure (leading <> [stem])

    dropAdjacentGlobStars (SlotGlobStar : SlotGlobStar : rest) = dropAdjacentGlobStars (SlotGlobStar : rest)
    dropAdjacentGlobStars (s : rest) = s : dropAdjacentGlobStars rest
    dropAdjacentGlobStars [] = []

{- | The literals a variable may sit between. The suffix opens with a capital
so that it is a boundary in every casing, and the whole affix is often empty,
which is the plain whole-segment variable.
-}
genAffix :: Gen Affix
genAffix =
    Affix
        <$> Gen.element ["", "use", "with"]
        <*> Gen.element ["", "ViewModel", "Container", "View"]

varNames :: [Text]
varNames = ["alpha", "beta", "gamma", "delta", "epsilon"]

{- | Fills every slot with segments. A globstar stands for zero to two
segments, each carrying a @.@ so that no variable could have captured it; a
trailing globstar gets at least one, since @a\/**@ needs something after the
slash.
-}
genPlanting :: [Slot] -> Gen Planting
genPlanting slots = Planting <$> traverse fill (withTrailing slots)
  where
    withTrailing ss = zip ss (drop 1 (fmap Just ss) <> [Nothing])

    fill (slot@(SlotLiteral t), _) = pure (slot, [t])
    fill (slot@SlotStar, _) = (slot,) . one <$> genOpaqueSegment
    fill (slot@(SlotVar name casing affix), _) = do
        value <- genValue
        spelling <- genRendering casing (value <> [head' name])
        pure (slot, [affix.prefix <> spelling <> affix.suffix])
    fill (slot@SlotGlobStar, next) = do
        let low = maybe 1 (const 0) next
        count <- Gen.int (Range.linear low 2)
        (slot,) <$> Gen.list (Range.singleton count) genOpaqueSegment

    head' [] = "x"
    head' (w : _) = w

{- | A path that may or may not match: sometimes exactly what was planted,
sometimes one segment dropped, inserted or replaced. Half the value of the
model is in the paths that must be rejected.
-}
genPerturbed :: Planting -> Gen [Text]
genPerturbed planting =
    Gen.frequency
        [ (1, pure planted)
        , (1, perturb)
        ]
  where
    planted = concatMap snd planting.segments

    perturb
        | null planted = one <$> genOpaqueSegment
        | otherwise = do
            index <- Gen.int (Range.linear 0 (length planted - 1))
            replacement <- Gen.choice [pure [], one <$> genOpaqueSegment, pure (take 2 (drop index planted))]
            pure (take index planted <> replacement <> drop (index + 1) planted)

-- | A segment no variable can capture, because every casing's class excludes @.@.
genOpaqueSegment :: Gen Text
genOpaqueSegment = do
    stem <- genWord
    pure (stem <> ".seg")

-- | A name of 2-5 words, every spelling of which is a legal pattern token.
genName :: Gen [Text]
genName = Gen.list (Range.linear 2 5) genWord

{- | A name of at least three words. Two words is the smallest legal name and
the shape every hand-written test reaches for, so the longer ones get a
generator of their own rather than being left to chance.
-}
genLongName :: Gen [Text]
genLongName = Gen.list (Range.linear 3 6) genWord

{- | A word for a variable /name/. The second character is a letter, which
rules out both ways a name can fail to compile: a Pascal spelling of @v4g8@
would be @V4G8@, which reads as CONSTANT_CASE too, and a one-letter word would
put two capitals side by side.
-}
genWord :: Gen Text
genWord = do
    opening <- Gen.lower
    following <- Gen.lower
    rest <- Gen.text (Range.linear 0 3) (Gen.choice [Gen.lower, Gen.digit])
    pure (T.singleton opening <> T.singleton following <> rest)

{- | A captured /value/ of 1-3 words. Values are read, never parsed as a
pattern, so they are free to be one letter long and to carry digits - which is
where @AB@, @Api2fa@ and @HTTP2_CLIENT@ live.
-}
genValue :: Gen [Text]
genValue = do
    opening <- genValueWord Gen.lower
    rest <- Gen.list (Range.linear 0 2) (genValueWord (Gen.choice [Gen.lower, Gen.digit]))
    pure (opening : rest)

-- | A value word: one character or more, opening with whatever is allowed here.
genValueWord :: Gen Char -> Gen Text
genValueWord genOpening = do
    opening <- genOpening
    rest <- Gen.text (Range.linear 0 4) (Gen.choice [Gen.lower, Gen.digit])
    pure (T.singleton opening <> rest)

{- | One of the ways a codebase might spell a name in a casing. Pascal and
camel may write any word wholly upper-case, which is what an acronym is.
Written by hand rather than by calling 'Deslop.Casing.renderings', so the
property is a comparison and not a restatement.
-}
genRendering :: Casing -> [Text] -> Gen Text
genRendering KebabCase name = pure (T.intercalate "-" name)
genRendering ConstantCase name = pure (T.intercalate "_" (T.toUpper <$> name))
genRendering PascalCase name = T.concat <$> traverse shout name
genRendering CamelCase name = case name of
    [] -> pure ""
    (first' : rest) -> (first' <>) . T.concat <$> traverse shout rest

shout :: Text -> Gen Text
shout word = Gen.element [capitalised, T.toUpper word]
  where
    capitalised = T.toUpper (T.take 1 word) <> T.drop 1 word

allCasings :: [Casing]
allCasings = [minBound .. maxBound]
