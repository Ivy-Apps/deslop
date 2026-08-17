{- | Glob+ patterns, and the matcher that runs them.

A Glob+ pattern is a list of /path segments/. Exactly one token, @**@, varies
how many segments the pattern consumes; everything else consumes one segment,
or part of one. That single fact is what the whole design rests on.

A target pattern may not put a variable between two @**@ - the compiler
rejects it - and so every variable's segment index is a function of the
pattern and the path length alone. The globstar search therefore decides only
/whether/ a path matches, never /what/ anything binds. The two searches here
are independent because of it:

* 'walkSegments' chooses globstar widths. Bindings are invariant under it, so
  its order is an implementation detail.
* 'bindParts' chooses how one segment's text divides between its parts. That
  order /is/ observable, and it is greedy-left: the leftmost part takes the
  most it can, and the first division satisfying every constraint wins.

Agreement between repeated occurrences is carried /through/ the walk rather
than checked after it. Each variable holds the set of names still able to
spell every occurrence seen so far; when that set empties, the branch dies
there. A kebab-case or CONSTANT_CASE occurrence collapses it to one name
immediately, which is why the common pattern costs nothing.

Compilation lives in "Deslop.GlobPlus.Compiler".
-}
module Deslop.GlobPlus (
    -- * Names
    Casing (..),
    VarName (..),
    CasedName (..),
    BoundName (..),
    casedAs,

    -- * Pattern structure
    Seg (..),
    SegPart (..),
    PatternSegment,
    TargetVar (..),
    ClauseVar (..),
    Polarity (..),

    -- * Compiled patterns
    CompiledTargetPattern (..),
    CompiledClausePattern (..),
    CompiledExcludePattern (..),

    -- * Paths
    Segments (..),
    segmentsOf,
    pathOf,

    -- * Matching
    MatchEnv (..),
    matchTarget,
    matchClause,
    matchExclude,

    -- * Clause hydration (hot path)
    ResolvedClause,
    hydrate,
    matchResolved,

    -- * Expansion
    moduleFromGlob,
    renderClausePattern,
    valueOf,
    spellVar,
    targetDirKeyword,

    -- * Shared with the compiler
    minSegments,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deslop.Casing (AgreedName (..), Casing (..), agree, decode, render, renderings)

--------------------------------------------------------------------------------
-- 1. Names
--------------------------------------------------------------------------------

{- | The identity of a variable, canonicalised to kebab-case words. All four
spellings of a name denote the same variable: @{{ProviderName}}@,
@{{providerName}}@, @{{provider-name}}@ and @{{PROVIDER_NAME}}@ are all
'VarName' @\"provider-name\"@.
-}
newtype VarName = VarName {text :: Text}
    deriving (Show, Eq, Ord)

-- | One value, available in every casing.
data CasedName = CasedName
    { pascal :: Text
    , camel :: Text
    , kebab :: Text
    , constant :: Text
    }
    deriving (Show, Eq)

{- | A bound variable: the name its occurrences agreed on, written out in every
casing, plus every name they could equally have denoted. The alternatives are
what a 'Widen' clause ranges over, where missing a match would be worse than
making a spurious one.
-}
data BoundName = BoundName
    { spelling :: CasedName
    , candidates :: NonEmpty [Text]
    }
    deriving (Show, Eq)

casedAs :: Casing -> BoundName -> Text
casedAs PascalCase n = n.spelling.pascal
casedAs CamelCase n = n.spelling.camel
casedAs KebabCase n = n.spelling.kebab
casedAs ConstantCase n = n.spelling.constant

--------------------------------------------------------------------------------
-- 2. Pattern structure
--------------------------------------------------------------------------------

{- | One piece of a pattern, at the level where a globstar lives. Parameterised
over what a single segment is, because a target segment binds variables while a
hydrated clause segment only decides.
-}
data Seg a
    = -- | @**@: zero or many whole segments.
      GlobStar
    | -- | Exactly one segment.
      Segment a
    deriving (Show, Eq, Functor, Foldable, Traversable)

{- | One piece of a single segment. The list of parts may be empty, which is
the empty segment - reachable both by writing @a\/\/b@ and by hydrating
@{{TARGET_DIR}}@ for a file that sits at the root.
-}
data SegPart var
    = Lit Text
    | -- | @*@: zero or more characters, never a @\/@.
      AnyChars
    | VarPart var
    deriving (Show, Eq, Functor, Foldable, Traversable)

type PatternSegment var = Seg [SegPart var]

-- | A variable occurrence in a target pattern. Strictly no @{{TARGET_DIR}}@.
data TargetVar = TargetVar VarName Casing
    deriving (Show, Eq, Ord)

-- | A variable occurrence in a clause pattern.
data ClauseVar
    = ClauseVar VarName Casing
    | -- | @{{TARGET_DIR}}@
      CTargetDir
    deriving (Show, Eq, Ord)

{- | Which way it is safe to be wrong when a variable's spelling has to be
guessed.

Deslop guesses in whichever direction costs a false positive rather than a
false negative, because a false positive is visible and baselineable while a
rule that quietly stops enforcing is not. A @forbids:@ pattern matching means a
violation, so it accepts every spelling of every name the capture could have
denoted. A @uses:@, @exists:@ or @allows:@ pattern matching means the rule is
/satisfied/, so widening it could only ever remove a report - those accept the
canonical spelling alone.
-}
data Polarity
    = -- | Accept every spelling: @target@ and @forbids@.
      Widen
    | -- | Accept the canonical spelling only: @uses@, @exists@, @allows@.
      Narrow
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 3. Compiled patterns
--------------------------------------------------------------------------------

data CompiledTargetPattern = CompiledTargetPattern
    { segments :: [PatternSegment TargetVar]
    , minLength :: Int
    , boundVars :: Set VarName
    , source :: Text
    }
    deriving (Show, Eq)

data CompiledClausePattern = CompiledClausePattern
    { segments :: [PatternSegment ClauseVar]
    , polarity :: Polarity
    , source :: Text
    }
    deriving (Show, Eq)

{- | An exclude pattern is a plain glob. 'Void' makes 'VarPart' uninhabited, so
a variable in an exclude pattern is unrepresentable rather than merely
rejected - and with no variable there is nothing to guess, which is why an
exclude carries no 'Polarity'.
-}
data CompiledExcludePattern = CompiledExcludePattern
    { segments :: [PatternSegment Void]
    , minLength :: Int
    , source :: Text
    }
    deriving (Show, Eq)

-- | How many segments a pattern must consume at minimum: an O(1) reject.
minSegments :: [Seg a] -> Int
minSegments = length . filter isSegment
  where
    isSegment GlobStar = False
    isSegment (Segment _) = True

--------------------------------------------------------------------------------
-- 4. Paths
--------------------------------------------------------------------------------

{- | A module path, split into segments once. Splitting at the call site is
what lets one module id be tested against every rule and clause without being
taken apart again each time.
-}
newtype Segments = Segments {segments :: [Text]}
    deriving (Show, Eq)

segmentsOf :: Text -> Segments
segmentsOf = Segments . T.splitOn "/"

pathOf :: Segments -> Text
pathOf = T.intercalate "/" . (.segments)

instance IsString Segments where
    fromString = segmentsOf . toText

data MatchEnv = MatchEnv
    { targetDir :: Text
    , variables :: Map VarName BoundName
    }
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- 5. Target matching
--------------------------------------------------------------------------------

matchTarget :: CompiledTargetPattern -> Segments -> Maybe MatchEnv
matchTarget target (Segments path)
    | length path < target.minLength = Nothing
    | otherwise = do
        bindings <- walkSegments bindParts target.segments path noBindings
        pure
            MatchEnv
                { targetDir = directoryOf path
                , variables = resolveBindings bindings
                }

-- | The directory a matched path sits in: everything but its final segment.
directoryOf :: [Text] -> Text
directoryOf = maybe "." (T.intercalate "/" . init) . nonEmpty

{- | The outer walk: how many segments each globstar eats.

Shared by targets, clauses and excludes, which differ only in what matching a
single segment /means/ - the @step@ argument. A step returns every way that
segment could be consumed, so that a later segment can reject an earlier
segment's division and the walk carries on to the next one.
-}
walkSegments :: (a -> Text -> st -> [st]) -> [Seg a] -> [Text] -> st -> Maybe st
walkSegments _ [] path st = st <$ guard (null path)
walkSegments step (GlobStar : rest) path st =
    asum [walkSegments step rest (drop width path) st | width <- [0 .. slack]]
  where
    slack = length path - minSegments rest
walkSegments step (Segment a : rest) path st = case path of
    [] -> Nothing
    (segment : deeper) -> asum [walkSegments step rest deeper st' | st' <- step a segment st]

--------------------------------------------------------------------------------
-- 6. Binding, and agreement as a constraint
--------------------------------------------------------------------------------

{- | What each variable has bound so far: every occurrence's literal text, and
the name they have agreed on given all of them.
-}
newtype Bindings = Bindings (Map VarName Binding)

data Binding = Binding
    { agreed :: AgreedName
    , occurrences :: NonEmpty (Casing, Text)
    }

noBindings :: Bindings
noBindings = Bindings Map.empty

{- | Every way the parts can divide one segment's text, greedy-left: each slot
in turn takes the most it can. A division that contradicts what a variable has
already bound is never returned, so the caller's search is pruned at the
earliest point the contradiction is visible rather than at the end.
-}
bindParts :: [SegPart TargetVar] -> Text -> Bindings -> [Bindings]
bindParts [] text bindings = [bindings | T.null text]
bindParts (Lit literal : rest) text bindings = case T.stripPrefix literal text of
    Just remaining -> bindParts rest remaining bindings
    Nothing -> []
bindParts (AnyChars : rest) text bindings =
    [ bound
    | taken <- widthsOf 0 text
    , bound <- bindParts rest (T.drop taken text) bindings
    ]
bindParts (VarPart (TargetVar name casing) : rest) text bindings =
    [ bound
    | taken <- widthsOf 1 text
    , let value = T.take taken text
    , capturedBy casing value
    , narrowed <- toList (bindOccurrence name casing value bindings)
    , bound <- bindParts rest (T.drop taken text) narrowed
    ]

-- | Candidate widths, longest first. A variable must take at least one char.
widthsOf :: Int -> Text -> [Int]
widthsOf smallest text = reverse [smallest .. T.length text]

{- | Records one occurrence and re-asks whether the variable's occurrences can
still denote one name. 'Nothing' is a dead branch: none can, so this division
of the path is not the intended one and the search moves on.

'Deslop.Casing.agree' is asked afresh over all the occurrences so far rather
than the previous answer being narrowed, and that matters. A name only some
occurrences /propose/ may still be spelled by all of them - @A00@ proposes
@a00@ and @a 00@, while @A_0_0@ proposes @a 0 0@, and it is the last of those
that spells both. Narrowing the first occurrence's proposals would lose it.
-}
bindOccurrence :: VarName -> Casing -> Text -> Bindings -> Maybe Bindings
bindOccurrence name casing value (Bindings bound) = do
    let occurrences = case Map.lookup name bound of
            Nothing -> one (casing, value)
            Just binding -> binding.occurrences <> one (casing, value)
    agreed <- agree occurrences
    pure . Bindings $ Map.insert name (Binding agreed occurrences) bound

{- | Whether a value is something this casing's capture accepts. Deliberately
looser than 'Deslop.Casing.spelledIn': a /pattern/ token must be a well-formed
spelling, but a /value/ read out of a codebase is taken as generously as the
character class allows.
-}
capturedBy :: Casing -> Text -> Bool
capturedBy PascalCase = opensWith isAsciiUpper isAlphaNum
capturedBy CamelCase = opensWith isAsciiLower isAlphaNum
capturedBy KebabCase = opensWith isKebabChar isKebabChar
capturedBy ConstantCase = opensWith isConstantChar isConstantChar

opensWith :: (Char -> Bool) -> (Char -> Bool) -> Text -> Bool
opensWith isFirst isRest text = case T.uncons text of
    Just (c, rest) -> isFirst c && T.all isRest rest
    Nothing -> False

isAlphaNum, isKebabChar, isConstantChar :: Char -> Bool
isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c
isKebabChar c = isAsciiLower c || isDigit c || c == '-'
isConstantChar c = isAsciiUpper c || isDigit c || c == '_'

{- | Turns the surviving constraints into values.

The coarsest surviving name - the one with fewest words - is what gets written
out, which is the standard acronym reading derived from the rendering model
rather than bolted on beside it. Each occurrence's own literal text is then
written back into its own casing slot, so same-casing use is always exact even
where the agreed name renders differently: a captured @HTTPClient@ stays
@HTTPClient@ in a Pascal clause.
-}
resolveBindings :: Bindings -> Map VarName BoundName
resolveBindings (Bindings bound) = resolve <$> bound
  where
    resolve binding =
        BoundName
            { spelling = foldl' overlay (casedNameFrom binding.agreed.canonical) (toList binding.occurrences)
            , candidates = binding.agreed.candidates
            }

    overlay named (PascalCase, value) = named {pascal = value}
    overlay named (CamelCase, value) = named {camel = value}
    overlay named (KebabCase, value) = named {kebab = value}
    overlay named (ConstantCase, value) = named {constant = value}

casedNameFrom :: [Text] -> CasedName
casedNameFrom name =
    CasedName
        { pascal = render PascalCase name
        , camel = render CamelCase name
        , kebab = render KebabCase name
        , constant = render ConstantCase name
        }

--------------------------------------------------------------------------------
-- 7. Clause and exclude matching
--------------------------------------------------------------------------------

{- | A clause pattern with its variables already substituted. Built once per
matched target and reused for every candidate path, which is the difference
between resolving a variable once and resolving it per import.
-}
data ResolvedClause = ResolvedClause
    { segments :: [Seg [ResolvedPart]]
    , minLength :: Int
    }
    deriving (Show, Eq)

-- | One piece of a hydrated segment: no variables left, only text to match.
data ResolvedPart
    = RLit Text
    | RAnyChars
    | -- | One of several spellings, as a 'Widen' clause admits.
      RAlt (NonEmpty Text)
    deriving (Show, Eq)

matchClause :: CompiledClausePattern -> MatchEnv -> Segments -> Bool
matchClause clause env = matchResolved (hydrate env clause)

matchExclude :: CompiledExcludePattern -> Segments -> Bool
matchExclude exclude (Segments path)
    | length path < exclude.minLength = False
    | otherwise = isJust (walkSegments matchParts (resolveVoid <$> exclude.segments) path ())
  where
    resolveVoid = fmap (fmap plainPart)
    plainPart (Lit t) = RLit t
    plainPart AnyChars = RAnyChars
    plainPart (VarPart v) = absurd v

matchResolved :: ResolvedClause -> Segments -> Bool
matchResolved clause (Segments path)
    | length path < clause.minLength = False
    | otherwise = isJust (walkSegments matchParts clause.segments path ())

-- | Whether the parts can divide this segment's text at all.
matchParts :: [ResolvedPart] -> Text -> () -> [()]
matchParts parts text () = [() | consumes parts text]
  where
    consumes [] remaining = T.null remaining
    consumes (RLit literal : rest) remaining = maybe False (consumes rest) (T.stripPrefix literal remaining)
    consumes (RAnyChars : rest) remaining =
        any (\taken -> consumes rest (T.drop taken remaining)) (widthsOf 0 remaining)
    consumes (RAlt spellings : rest) remaining =
        any (\s -> maybe False (consumes rest) (T.stripPrefix s remaining)) spellings

{- | Substitutes a clause's variables. @{{TARGET_DIR}}@ is the one substitution
that can introduce a @\/@, so a hydrated segment may become several - which is
why hydration produces the segment list rather than editing it in place.
-}
hydrate :: MatchEnv -> CompiledClausePattern -> ResolvedClause
hydrate env clause =
    ResolvedClause
        { segments = hydrated
        , minLength = minSegments hydrated
        }
  where
    hydrated = concatMap hydrateSegment clause.segments

    hydrateSegment GlobStar = [GlobStar]
    hydrateSegment (Segment parts) = Segment <$> splitOnSlash (mergeLits (hydratePart =<< parts))

    hydratePart (Lit t) = [RLit t]
    hydratePart AnyChars = [RAnyChars]
    hydratePart (VarPart CTargetDir) = [RLit env.targetDir]
    hydratePart (VarPart (ClauseVar name casing)) = case Map.lookup name env.variables of
        -- Compilation guarantees every clause variable is bound, so this is
        -- unreachable. Matching nothing keeps an impossible state from widening.
        Nothing -> [RAlt (one "\0unbound")]
        Just bound -> [spellingsOf clause.polarity casing bound]

    mergeLits (RLit a : RLit b : rest) = mergeLits (RLit (a <> b) : rest)
    mergeLits (part : rest) = part : mergeLits rest
    mergeLits [] = []

{- | What a variable stands for in a clause, in the direction its polarity says
it is safe to be wrong.
-}
spellingsOf :: Polarity -> Casing -> BoundName -> ResolvedPart
spellingsOf Narrow casing bound = RLit (casedAs casing bound)
spellingsOf Widen casing bound = case nonEmpty (take alternationLimit spellings) of
    Just alternatives -> RAlt alternatives
    Nothing -> RLit (casedAs casing bound)
  where
    -- The literal capture first, so a name pathological enough to be truncated
    -- still matches the way it was actually captured.
    spellings = ordNub (casedAs casing bound : concatMap (toList . renderings casing) bound.candidates)

{- | A cap on how wide a 'Widen' clause may grow. A name with more acronym
letters than this is pathological rather than real.
-}
alternationLimit :: Int
alternationLimit = 256

{- | Breaks a hydrated segment wherever a substitution introduced a @\/@. Only
@{{TARGET_DIR}}@ can, since no capture's character class admits one.
-}
splitOnSlash :: [ResolvedPart] -> [[ResolvedPart]]
splitOnSlash = go []
  where
    go current [] = [reverse current]
    go current (RLit text : rest) = case reverse (T.splitOn "/" text) of
        [] -> go current rest
        (final : beforeFinal) -> case reverse beforeFinal of
            -- No slash in the substitution: it stays part of this segment.
            [] -> go (RLit final : current) rest
            (opening : middles) ->
                reverse (RLit opening : current)
                    : fmap (one . RLit) middles
                    <> go [RLit final] rest
    go current (part : rest) = go (part : current) rest

--------------------------------------------------------------------------------
-- 8. Expansion
--------------------------------------------------------------------------------

{- | Expands a clause pattern into a concrete module path by substituting
variables. Returns Nothing if the pattern contains wildcards, which cannot be
deterministically expanded.
-}
moduleFromGlob :: MatchEnv -> CompiledClausePattern -> Maybe Text
moduleFromGlob env clause = T.intercalate "/" <$> traverse expandSegment clause.segments
  where
    expandSegment GlobStar = Nothing
    expandSegment (Segment parts) = T.concat <$> traverse expandPart parts

    expandPart (Lit t) = Just t
    expandPart AnyChars = Nothing
    expandPart (VarPart v) = valueOf env v

{- | Renders a clause pattern for a human, substituting what is bound and
keeping the wildcards literally.
-}
renderClausePattern :: MatchEnv -> CompiledClausePattern -> Text
renderClausePattern env clause = T.intercalate "/" (renderSegment <$> clause.segments)
  where
    renderSegment GlobStar = "**"
    renderSegment (Segment parts) = T.concat (renderPart <$> parts)

    renderPart (Lit t) = t
    renderPart AnyChars = "*"
    renderPart (VarPart v) = fromMaybe (asWritten v) (valueOf env v)

    asWritten CTargetDir = "{{" <> targetDirKeyword <> "}}"
    asWritten (ClauseVar name casing) = "{{" <> spellVar name casing <> "}}"

-- | What a clause variable stands for under a match, if anything does.
valueOf :: MatchEnv -> ClauseVar -> Maybe Text
valueOf env CTargetDir = Just env.targetDir
valueOf env (ClauseVar name casing) = casedAs casing <$> Map.lookup name env.variables

-- | Writes a variable's canonical name back out in the given casing.
spellVar :: VarName -> Casing -> Text
spellVar name casing = render casing (decode KebabCase name.text)

targetDirKeyword :: Text
targetDirKeyword = "TARGET_DIR"
