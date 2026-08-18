{- | A brute-force reference implementation of Glob+ target matching, written
to be obviously correct rather than fast.

The oracle enumerates /every/ way a pattern can consume a path - every width
for every globstar, every split of every segment into its parts - and returns
the first assignment, in greedy-left order, on which all occurrences of each
variable agree. Production has to be clever about this; the oracle does not,
which is the whole point of comparing them.

What is /not/ re-implemented here is the casing layer: 'Deslop.Casing.decodings'
and 'Deslop.Casing.spells' are used as given. That layer is unchanged by this
rework and has its own property suite, and reusing it is what isolates the
claim these properties make, which is about /structure/ - which segment a
variable binds to, and which split of a segment wins.
-}
module Deslop.GlobPlusOracle (
    -- * The model
    OPart (..),
    OSeg (..),
    OPattern,
    Capture (..),
    renderOPattern,
    oracleMatch,
    oracleMatches,
    oracleAgreeing,
    unambiguousSegment,
    capturedBy,

    -- * Legality, as the compiler defines it
    isLegalTarget,
    unanchoredVars,
    unboundedSegments,
    parentDirsLegal,

    -- * Parent directories
    resolveParentDirs,

    -- * Generators
    genOPattern,
    genOSegs,
    genPathFor,
    genPlantingFor,
    plantedPath,
    plantedOffset,
    genPerturbedPath,
    genOpaqueSegment,
    genWord,
    genVarName,
    genLongVarName,
    genValue,
    genRendering,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List qualified as L
import Data.Text qualified as T
import Deslop.Casing (Casing (..), decodings, render, spells)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------
-- The model
--------------------------------------------------------------------------------

-- | One piece of a single path segment.
data OPart
    = OLit Text
    | -- | @*@: zero or more characters, never a @\/@.
      OStar
    | OVar [Text] Casing
    deriving (Show, Eq)

-- | One piece of a pattern, at the level where a globstar lives.
data OSeg
    = -- | @**@: zero or many whole segments.
      OGlobStar
    | -- | Exactly one segment, split into parts.
      OSeg [OPart]
    deriving (Show, Eq)

type OPattern = [OSeg]

-- | One variable occurrence, and the text it took.
data Capture = Capture
    { name :: [Text]
    , casing :: Casing
    , value :: Text
    }
    deriving (Show, Eq, Ord)

renderOPattern :: OPattern -> Text
renderOPattern = T.intercalate "/" . fmap segText
  where
    segText OGlobStar = "**"
    segText (OSeg parts) = T.concat (partText <$> parts)

    partText (OLit t) = t
    partText OStar = "*"
    partText (OVar name casing) = "{{" <> render casing name <> "}}"

--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------

{- | The first assignment on which every variable's occurrences agree, in
greedy-left order. 'Nothing' means no assignment does, which is what a
non-match is.
-}
oracleMatch :: OPattern -> [Text] -> Maybe [Capture]
oracleMatch pattern' = find agreesEverywhere . oracleMatches pattern'

{- | Every assignment that satisfies the pattern structurally, in the order the
production matcher is specified to try them: greedy-left within a segment.

Globstar widths are enumerated shortest-first, but the order is immaterial:
no variable may have a globstar on both sides, so every variable's segment
index is fixed by the pattern and the path length, and all of these
assignments necessarily agree about which segment each variable took.
-}
oracleAgreeing :: OPattern -> [Text] -> [[Capture]]
oracleAgreeing pattern' = filter agreesEverywhere . oracleMatches pattern'

{- | Whether a segment divides into its parts in only one way. A segment with
two variables, or with a variable beside a @*@, does not: several divisions can
satisfy it and they bind different text. Globstar widths never do that, which
is the difference the anchoring rule buys.
-}
unambiguousSegment :: OSeg -> Bool
unambiguousSegment OGlobStar = True
unambiguousSegment (OSeg parts) = length variables <= 1 && (null variables || notElem OStar parts)
  where
    variables = [() | OVar _ _ <- parts]

oracleMatches :: OPattern -> [Text] -> [[Capture]]
oracleMatches [] segments = [[] | null segments]
oracleMatches (OGlobStar : rest) segments =
    [ captures
    | width <- [0 .. length segments]
    , captures <- oracleMatches rest (drop width segments)
    ]
oracleMatches (OSeg parts : rest) segments = case segments of
    [] -> []
    (segment : more) ->
        [ here <> later
        | here <- matchParts parts segment
        , later <- oracleMatches rest more
        ]

{- | Every way the parts can divide one segment's text, longest-first for each
slot in turn - which is what "the leftmost variable takes the most it can"
means when it is read as an enumeration order.
-}
matchParts :: [OPart] -> Text -> [[Capture]]
matchParts [] text = [[] | T.null text]
matchParts (OLit literal : rest) text = case T.stripPrefix literal text of
    Just remaining -> matchParts rest remaining
    Nothing -> []
matchParts (OStar : rest) text =
    [ captures
    | taken <- widthsOf 0 text
    , captures <- matchParts rest (T.drop taken text)
    ]
matchParts (OVar name casing : rest) text =
    [ Capture name casing value : captures
    | taken <- widthsOf 1 text
    , let value = T.take taken text
    , capturable casing value
    , captures <- matchParts rest (T.drop taken text)
    ]

-- | Candidate widths, longest first. A variable must take at least one char.
widthsOf :: Int -> Text -> [Int]
widthsOf smallest text = reverse [smallest .. T.length text]

{- | Whether every variable's occurrences could denote one name. Exactly the
question 'Deslop.Casing.agree' answers, asked here without reference to how
production carries it.
-}
agreesEverywhere :: [Capture] -> Bool
agreesEverywhere captures = not (any (null . survivingNames) (groupByName captures))

-- | The names that spell every occurrence in a group.
survivingNames :: [Capture] -> [[Text]]
survivingNames occurrences =
    [ candidate
    | candidate <- ordNub (concatMap (toList . proposals) occurrences)
    , all (\c -> spells c.casing candidate c.value) occurrences
    ]
  where
    proposals c = decodings c.casing c.value

groupByName :: [Capture] -> [[Capture]]
groupByName = L.groupBy ((==) `on` (.name)) . L.sortOn (.name)

-- | What one variable took, by the name it is written under in the pattern.
capturedBy :: [Text] -> [Capture] -> [Text]
capturedBy name = fmap (.value) . filter ((== name) . (.name))

-- | Whether a segment is something the given casing could have captured.
capturable :: Casing -> Text -> Bool
capturable PascalCase = charClass isAsciiUpper isAlphaNum
capturable CamelCase = charClass isAsciiLower isAlphaNum
capturable KebabCase = charClass isKebabChar isKebabChar
capturable ConstantCase = charClass isConstantChar isConstantChar

charClass :: (Char -> Bool) -> (Char -> Bool) -> Text -> Bool
charClass isFirst isRest text = case T.uncons text of
    Just (c, rest) -> isFirst c && T.all isRest rest
    Nothing -> False

isAlphaNum, isKebabChar, isConstantChar :: Char -> Bool
isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c
isKebabChar c = isAsciiLower c || isDigit c || c == '-'
isConstantChar c = isAsciiUpper c || isDigit c || c == '_'

--------------------------------------------------------------------------------
-- Legality
--------------------------------------------------------------------------------

-- | Whether the compiler should accept this pattern as a target.
isLegalTarget :: OPattern -> Bool
isLegalTarget pattern' = null (unanchoredVars pattern') && null (unboundedSegments pattern')

{- | Every variable with a globstar on both sides. Such a variable's segment
index is decided by the path rather than by the pattern, so it has no meaning.
-}
unanchoredVars :: OPattern -> [[Text]]
unanchoredVars pattern' =
    ordNub
        [ name
        | (index, OSeg parts) <- indexed
        , any (isGlobStarAt (< index)) indexed
        , any (isGlobStarAt (> index)) indexed
        , OVar name _ <- parts
        ]
  where
    indexed = zip [0 :: Int ..] pattern'
    isGlobStarAt side (index, segment) = side index && segment == OGlobStar

{- | Whether every @..@ in a clause, written as raw segments, goes back past
something that names one directory. Written over the text rather than over a
compiled pattern, so it is a second opinion and not a restatement of the
structure production walks.
-}
parentDirsLegal :: [Text] -> Bool
parentDirsLegal = isRight . foldlM back []
  where
    back behind ".." = case behind of
        -- Nothing left to go back past: the clamp, and legal.
        [] -> Right []
        (segment : earlier)
            | "*" `T.isInfixOf` segment -> Left ()
            | otherwise -> Right earlier
    back behind segment = Right (segment : behind)

{- | Resolving @..@ over plain segments, written the obvious way: each one
takes back the segment before it, and one with nothing before it does nothing.

Production has to do this over a pattern whose steps expand to varying numbers
of segments - @{{TARGET_DIR}}@ is one step and many segments - which is the
difference a differential property is measuring.
-}
resolveParentDirs :: [Text] -> [Text]
resolveParentDirs = reverse . foldl' back []
  where
    back done ".." = drop 1 done
    back done segment = segment : done

-- | Every segment holding two variables with no literal between them.
unboundedSegments :: OPattern -> [[OPart]]
unboundedSegments pattern' =
    [parts | OSeg parts <- pattern', hasUnseparatedPair (filter (/= OStar) parts)]
  where
    hasUnseparatedPair (OVar {} : OVar {} : _) = True
    hasUnseparatedPair (_ : rest) = hasUnseparatedPair rest
    hasUnseparatedPair [] = False

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

{- | A legal target pattern. Segments are generated freely and then repaired,
rather than generated within the rules, so that the shapes the rules exist to
forbid are actually reached and then fixed in a way that keeps them varied.
-}
genOPattern :: Gen OPattern
genOPattern = repairBoundaries . repairAnchoring <$> genOSegs

genOSegs :: Gen OPattern
genOSegs = do
    count <- Gen.int (Range.linear 1 5)
    dropAdjacentGlobStars <$> Gen.list (Range.singleton count) genOSeg

genOSeg :: Gen OSeg
genOSeg =
    Gen.frequency
        [ (1, pure OGlobStar)
        , (1, pure (OSeg [OStar]))
        , (2, OSeg . one . OLit <$> Gen.element ["app", "features", "components", "lib"])
        , (4, OSeg <$> genParts)
        ]

-- | A segment carrying one or two variables, with optional literal affixes.
genParts :: Gen [OPart]
genParts = do
    leading <- Gen.element [[], [OLit "use"], [OLit "with"]]
    first' <- genVarPart
    trailing <- Gen.element [[], [OLit "View"], [OLit "ViewModel"], [OLit ".spec"]]
    extra <-
        Gen.frequency
            [ (3, pure [])
            , (1, (\v -> [OLit "-", v]) <$> genVarPart)
            , (1, (\v -> [OStar, v]) <$> genVarPart)
            ]
    pure (leading <> [first'] <> extra <> trailing)

genVarPart :: Gen OPart
genVarPart = OVar <$> genVarName <*> Gen.element [minBound .. maxBound]

{- | Turns every unanchored variable into an anchored one by demoting the
nearest globstar that follows it into a @*@, which fixes that depth. Removing
a globstar strictly decreases their number, so this terminates.
-}
repairAnchoring :: OPattern -> OPattern
repairAnchoring pattern' = case firstUnanchoredIndex pattern' of
    Nothing -> pattern'
    Just index -> repairAnchoring (demoteFirstGlobStarAfter index pattern')

firstUnanchoredIndex :: OPattern -> Maybe Int
firstUnanchoredIndex pattern' =
    viaNonEmpty head
        [ index
        | (index, OSeg parts) <- indexed
        , any isVar parts
        , any (\(i, s) -> i < index && s == OGlobStar) indexed
        , any (\(i, s) -> i > index && s == OGlobStar) indexed
        ]
  where
    indexed = zip [0 :: Int ..] pattern'
    isVar (OVar _ _) = True
    isVar _ = False

demoteFirstGlobStarAfter :: Int -> OPattern -> OPattern
demoteFirstGlobStarAfter index pattern' =
    case L.elemIndex OGlobStar (drop (index + 1) pattern') of
        Nothing -> pattern'
        Just offset ->
            let at = index + 1 + offset
             in take at pattern' <> [OSeg [OStar]] <> drop (at + 1) pattern'

{- | Puts a literal between two variables that have none, which is the only
thing that makes the boundary between them determinable.
-}
repairBoundaries :: OPattern -> OPattern
repairBoundaries = fmap repairSeg
  where
    repairSeg OGlobStar = OGlobStar
    repairSeg (OSeg parts) = OSeg (separate parts)

    separate (v@(OVar _ _) : rest) = case dropWhile (== OStar) rest of
        (OVar {} : _) -> v : OLit "-" : separate (dropWhile (== OStar) rest)
        _ -> v : separate rest
    separate (part : rest) = part : separate rest
    separate [] = []

dropAdjacentGlobStars :: OPattern -> OPattern
dropAdjacentGlobStars (OGlobStar : OGlobStar : rest) = dropAdjacentGlobStars (OGlobStar : rest)
dropAdjacentGlobStars (segment : rest) = segment : dropAdjacentGlobStars rest
dropAdjacentGlobStars [] = []

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

-- | A path the pattern is meant to match, by filling every slot.
genPathFor :: OPattern -> Gen [Text]
genPathFor = fmap plantedPath . genPlantingFor

-- | The segments each pattern slot was filled with, kept alongside the slot so
-- that a property can say where in the path a given slot's text begins.
genPlantingFor :: OPattern -> Gen [(OSeg, [Text])]
genPlantingFor = traverse (\slot -> (slot,) <$> fill slot)
  where
    fill OGlobStar = do
        width <- Gen.int (Range.linear 0 2)
        Gen.list (Range.singleton width) genOpaqueSegment
    fill (OSeg parts) = one . T.concat <$> traverse fillPart parts

    fillPart (OLit literal) = pure literal
    fillPart OStar = Gen.element ["", "x", "zz"]
    fillPart (OVar name casing) = genRendering casing =<< genValueFor name

plantedPath :: [(OSeg, [Text])] -> [Text]
plantedPath = concatMap snd

-- | Where in the planted path the segments for pattern slot @index@ begin.
plantedOffset :: Int -> [(OSeg, [Text])] -> Int
plantedOffset index = length . plantedPath . take index

{- | A path that may or may not match: sometimes exactly what was planted,
sometimes with one segment dropped, inserted or duplicated. Half the value of
a model is in the paths it must reject.
-}
genPerturbedPath :: [Text] -> Gen [Text]
genPerturbedPath planted =
    Gen.frequency
        [ (2, pure planted)
        , (1, perturb)
        ]
  where
    perturb
        | null planted = one <$> genOpaqueSegment
        | otherwise = do
            index <- Gen.int (Range.linear 0 (length planted - 1))
            replacement <-
                Gen.choice
                    [ pure []
                    , one <$> genOpaqueSegment
                    , pure (take 2 (drop index planted))
                    ]
            pure (take index planted <> replacement <> drop (index + 1) planted)

-- | A segment carrying a @.@, which no variable's character class accepts.
genOpaqueSegment :: Gen Text
genOpaqueSegment = (<> ".seg") <$> genWord

--------------------------------------------------------------------------------
-- Names and values
--------------------------------------------------------------------------------

{- | A variable name of 2-4 words. Two is the smallest that is unambiguous in
every casing, which is what makes every spelling of it a legal pattern token.
-}
genVarName :: Gen [Text]
genVarName = do
    count <- Gen.int (Range.linear 2 4)
    Gen.list (Range.singleton count) genWord

{- | A name of at least three words. Two is the smallest legal name and the
shape every hand-written case reaches for, so the longer ones get a generator
of their own rather than being left to chance.
-}
genLongVarName :: Gen [Text]
genLongVarName = do
    count <- Gen.int (Range.linear 3 6)
    Gen.list (Range.singleton count) genWord

{- | A captured /value/ of 1-3 words. Values are read rather than parsed, so
they may be one letter long and carry digits - which is where @AB@, @Api2fa@
and @HTTP2_CLIENT@ live.
-}
genValue :: Gen [Text]
genValue = do
    opening <- genValueWord
    rest <- Gen.list (Range.linear 0 2) genValueWord
    pure (opening : rest)

{- | A word for a variable /name/. The second character is a letter, which
rules out both ways a name fails to compile: a Pascal spelling of @v4g8@ reads
as CONSTANT_CASE too, and a one-letter word puts two capitals side by side.
-}
genWord :: Gen Text
genWord = do
    opening <- Gen.lower
    following <- Gen.lower
    rest <- Gen.text (Range.linear 0 3) (Gen.choice [Gen.lower, Gen.digit])
    pure (T.singleton opening <> T.singleton following <> rest)

{- | A captured /value/ of 1-3 words, ending in the variable's own last word so
that a value can be attributed to the variable that captured it. Values are
read rather than parsed, so they may be one letter long and carry digits -
which is where @AB@, @Api2fa@ and @HTTP2_CLIENT@ live.
-}
genValueFor :: [Text] -> Gen [Text]
genValueFor name = do
    leading <- Gen.list (Range.linear 0 2) genValueWord
    pure (leading <> [fromMaybe "x" (viaNonEmpty last name)])

genValueWord :: Gen Text
genValueWord = do
    opening <- Gen.lower
    rest <- Gen.text (Range.linear 0 3) (Gen.choice [Gen.lower, Gen.digit])
    pure (T.singleton opening <> rest)

{- | One of the ways a codebase might spell a name in a casing. Pascal and
camel may write any word wholly upper-case, which is what an acronym is.
Written by hand rather than by calling 'Deslop.Casing.renderings', so that a
property comparing them is a comparison and not a restatement.
-}
genRendering :: Casing -> [Text] -> Gen Text
genRendering KebabCase name = pure (T.intercalate "-" name)
genRendering ConstantCase name = pure (T.intercalate "_" (T.toUpper <$> name))
genRendering PascalCase name = T.concat <$> traverse shout name
genRendering CamelCase name = case name of
    [] -> pure ""
    (first' : rest) -> (first' <>) . T.concat <$> traverse shout rest

shout :: Text -> Gen Text
shout word = Gen.element [T.toUpper (T.take 1 word) <> T.drop 1 word, T.toUpper word]
