{- | The property suite for Glob+ matching: the main line of defence.

Properties are numbered P0-P22 and the numbers are load-bearing - they are the
ones agreed in the design session and referenced from
@docs/adr/0009-glob-plus-matches-path-segments.md@. P0 is the differential test
against 'Deslop.GlobPlusOracle' and subsumes much of the rest; the others are
kept anyway, because a failing P6 says "a variable bound the wrong segment"
where a failing P0 only says "the two disagree".
-}
module Deslop.GlobPlusPropSpec (spec) where

import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Casing (render, renderings, spells)
import Deslop.GlobPlus
import Deslop.GlobPlusOracle (OPart (..), OPattern, OSeg (..))
import Deslop.GlobPlusOracle qualified as O
import Hedgehog (Gen, MonadTest, PropertyT, annotate, annotateShow, assert, discard, failure, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.GlobPlus properties" $ do
    tier1
    tier2
    tier3
    tier4
    tier5
    tier6

--------------------------------------------------------------------------------
-- Tier 1: the oracle
--------------------------------------------------------------------------------

tier1 :: Spec
tier1 = describe "tier 1 - differential" $ do
    prop "P0 agrees with the brute-force oracle, on the decision and on every binding" $ do
        pattern' <- forAll O.genOPattern
        planted <- forAll (O.genPathFor pattern')
        path <- forAll (O.genPerturbedPath planted)
        compiled <- compileOrFail pattern'
        let expected = O.oracleMatch pattern' path
            actual = matchTarget compiled (joinPath path)
        annotateShow expected
        isJust actual === isJust expected
        case (actual, expected) of
            (Just env, Just captures) -> bindingsAgree env captures
            _ -> pure ()

--------------------------------------------------------------------------------
-- Tier 2: monotonicity
--------------------------------------------------------------------------------

tier2 :: Spec
tier2 = describe "tier 2 - monotonicity" $ do
    prop "P1 inserting ** anywhere preserves every match" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        widened <- forAllLegalInsertion pattern'
        assertMatches widened path

    prop "P2 inserting ** and any number of directories at that point preserves the match" $ do
        pattern' <- forAll O.genOPattern
        planting <- forAll (O.genPlantingFor pattern')
        index <- forAll (Gen.int (Range.linear 0 (length pattern')))
        let widened = insertAt index OGlobStar pattern'
        unless (O.isLegalTarget widened) discard
        inserted <- forAll (Gen.list (Range.linear 0 3) O.genOpaqueSegment)
        let offset = O.plantedOffset index planting
            path = O.plantedPath planting
            grown = take offset path <> inserted <> drop offset path
        assertMatches widened grown

    prop "P3 ** is idempotent: a doubled globstar says what one says" $ do
        pattern' <- forAll O.genOPattern
        planted <- forAll (O.genPathFor pattern')
        path <- forAll (O.genPerturbedPath planted)
        case L.elemIndex OGlobStar pattern' of
            Nothing -> discard
            Just index -> do
                let doubled = insertAt index OGlobStar pattern'
                one' <- matchOf pattern' path
                two <- matchOf doubled path
                one' === two

    prop "P4 widening ladder: a literal or a variable segment may become *, and * may become **" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        index <- forAll (Gen.int (Range.linear 0 (max 0 (length pattern' - 1))))
        case drop index pattern' of
            (OGlobStar : _) -> discard
            (OSeg [OStar] : _) -> do
                let widened = replaceAt index OGlobStar pattern'
                unless (O.isLegalTarget widened) discard
                assertMatches widened path
            (OSeg _ : _) -> assertMatches (replaceAt index (OSeg [OStar]) pattern') path
            [] -> discard

    prop "P5 a trailing ** stands for nothing, so a/** matches a" $ do
        pattern' <- forAll (Gen.filter (notElem OGlobStar) O.genOPattern)
        path <- forAll (O.genPathFor pattern')
        assertMatches (pattern' <> [OGlobStar]) path

--------------------------------------------------------------------------------
-- Tier 3: bindings
--------------------------------------------------------------------------------

tier3 :: Spec
tier3 = describe "tier 3 - bindings" $ do
    prop "P6 a variable's segment is fixed by the pattern and the path length, never by the split" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        compiled <- compileOrFail pattern'
        case matchTarget compiled (joinPath path) of
            Nothing -> discard
            Just env -> traverse_ (assertBoundInItsOwnSegment env path) (pinnedOccurrences pattern' (length path))

    prop "P7 every successful split yields identical bindings" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        let assignments = ordNub (fmap L.sort (O.oracleMatches pattern' path))
        when (length assignments < 2) discard
        annotateShow assignments
        length (ordNub (fmap capturedValues assignments)) === 1

    prop "P8 if any assignment satisfies the pattern, the matcher finds one" $ do
        pattern' <- forAll O.genOPattern
        planted <- forAll (O.genPathFor pattern')
        path <- forAll (O.genPerturbedPath planted)
        case O.oracleMatch pattern' path of
            Nothing -> discard
            Just _ -> assertMatches pattern' path

    prop "P9 every binding returned is spelled by every one of its occurrences" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        compiled <- compileOrFail pattern'
        case matchTarget compiled (joinPath path) of
            Nothing -> discard
            Just env -> traverse_ (assertSpelledByOccurrences env) (occurrencesOf pattern')

    prop "P10 planted values are recovered exactly" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        compiled <- compileOrFail pattern'
        expected <- maybe discard pure (O.oracleMatch pattern' path)
        env <- matchOrFail compiled path
        bindingsAgree env expected

--------------------------------------------------------------------------------
-- Tier 4: the compiler
--------------------------------------------------------------------------------

tier4 :: Spec
tier4 = describe "tier 4 - compiler" $ do
    prop "P11 legal patterns compile and illegal ones are rejected, each for its own reason" $ do
        pattern' <- forAll O.genOSegs
        let rendered = O.renderOPattern pattern'
        annotate (toString rendered)
        case compileTargetPattern rendered of
            Right _ -> assert (O.isLegalTarget pattern')
            Left err -> do
                annotate (toString (renderGlobPlusError err))
                assert (not (O.isLegalTarget pattern'))

    prop "P12 a compiled pattern's source recompiles to the same behaviour" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        compiled <- compileOrFail pattern'
        recompiled <- either (const failure) pure (compileTargetPattern compiled.source)
        matchTarget compiled (joinPath path) === matchTarget recompiled (joinPath path)

    prop "P13 boundVars is exactly the set of variable names in the pattern" $ do
        pattern' <- forAll O.genOPattern
        compiled <- compileOrFail pattern'
        boundVars compiled === Set.fromList (VarName . render KebabCase <$> namesOf pattern')

    prop "P14 compilation is total on arbitrary text" $ do
        raw <- forAll (Gen.text (Range.linear 0 40) (Gen.element patternChars))
        assert (isLeft (compileTargetPattern raw) || isRight (compileTargetPattern raw))

--------------------------------------------------------------------------------
-- Tier 5: polarity and casing
--------------------------------------------------------------------------------

tier5 :: Spec
tier5 = describe "tier 5 - polarity and casing" $ do
    prop "P15 Narrow is contained in Widen" $ do
        (env, name, casing) <- forAll genBoundEnv
        candidate <- forAll (genSpellingOf env name casing)
        let clause polarity = unsafeClause polarity (boundOf env) ("@/x/" <> braced (render casing name))
            path = "@/x/" <> candidate
        when (matchClause (clause Requiring) env path) $
            assert (matchClause (clause Forbidding) env path)

    prop "P16 same-casing use is exact under both polarities" $ do
        (env, name, casing) <- forAll genBoundEnv
        literal <- maybe discard pure (casedAs casing <$> Map.lookup (varNameOf name) env.variables)
        for_ [Requiring, Forbidding] $ \polarity ->
            assert (matchClause (unsafeClause polarity (boundOf env) ("@/x/" <> braced (render casing name))) env ("@/x/" <> literal))

    prop "P17 Widen accepts every spelling of every name the capture could denote" $ do
        (env, name, casing) <- forAll genBoundEnv
        bound <- maybe discard pure (Map.lookup (varNameOf name) env.variables)
        candidate <- forAll (Gen.element (spellingsOf casing bound))
        assert (matchClause (unsafeClause Forbidding (boundOf env) ("@/x/" <> braced (render casing name))) env ("@/x/" <> candidate))

    prop "P18 occurrences agree when some name spells both, not when both decode alike" $ do
        name <- forAll O.genVarName
        kebab <- forAll (O.genRendering KebabCase name)
        pascal <- forAll (O.genRendering PascalCase name)
        compiled <- either (const failure) pure (compileTargetPattern "@/c/{{provider-name}}/{{ProviderName}}View")
        annotate (toString (kebab <> "/" <> pascal))
        assert (isJust (matchTarget compiled ("@/c/" <> kebab <> "/" <> pascal <> "View")))

--------------------------------------------------------------------------------
-- Tier 6: robustness
--------------------------------------------------------------------------------

tier6 :: Spec
tier6 = describe "tier 6 - robustness" $ do
    prop "P19 matching is total on any pattern and any path" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (Gen.list (Range.linear 0 8) O.genOpaqueSegment)
        compiled <- compileOrFail pattern'
        assert (isJust (matchTarget compiled (joinPath path)) || True)

    prop "P20 globstar-heavy patterns against long paths still terminate" $ do
        depth <- forAll (Gen.int (Range.linear 10 24))
        path <- forAll (Gen.list (Range.singleton depth) O.genOpaqueSegment)
        compiled <- either (const failure) pure (compileTargetPattern "@/**/a/**/b/**/c/**/{{FileName}}View")
        assert (isNothing (matchTarget compiled (joinPath path)))

    prop "P21 TARGET_DIR is the matched path minus its final segment" $ do
        pattern' <- forAll O.genOPattern
        path <- forAll (O.genPathFor pattern')
        compiled <- compileOrFail pattern'
        case matchTarget compiled (joinPath path) of
            Nothing -> discard
            Just env -> env.targetDir === expectedTargetDir path

    prop "P22 an exclude pattern decides exactly as the same pattern would as a target" $ do
        pattern' <- forAll (Gen.filter (null . namesOf) O.genOPattern)
        planted <- forAll (O.genPathFor pattern')
        path <- forAll (O.genPerturbedPath planted)
        let rendered = O.renderOPattern pattern'
        target <- either (const failure) pure (compileTargetPattern rendered)
        excluded <- either (const failure) pure (compileExcludePattern rendered)
        matchExclude excluded (joinPath path) === isJust (matchTarget target (joinPath path))

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

-- | Production bound every variable to exactly what the oracle captured.
bindingsAgree :: (MonadTest m) => MatchEnv -> [O.Capture] -> m ()
bindingsAgree env = traverse_ agrees
  where
    agrees capture =
        (casedAs capture.casing <$> Map.lookup (varNameOf capture.name) env.variables)
            === Just capture.value

{- | The bound value sits in the segment the pattern pins it to. A variable
with no globstar before it is at a fixed index from the start; one with no
globstar after it is at a fixed index from the end. It cannot be neither,
because that is the shape the compiler rejects.
-}
assertBoundInItsOwnSegment :: (MonadTest m) => MatchEnv -> [Text] -> (Int, [Text], Casing) -> m ()
assertBoundInItsOwnSegment env path (index, name, casing) =
    case (Map.lookup (varNameOf name) env.variables, path !!? index) of
        (Just bound, Just segment) -> assert (casedAs casing bound `T.isInfixOf` segment)
        _ -> failure

assertSpelledByOccurrences :: (MonadTest m) => MatchEnv -> ([Text], Casing) -> m ()
assertSpelledByOccurrences env (name, casing) = case Map.lookup (varNameOf name) env.variables of
    Nothing -> failure
    Just bound -> assert (any (\candidate -> spells casing candidate (casedAs casing bound)) bound.candidates)

assertMatches :: (MonadTest m) => OPattern -> [Text] -> m ()
assertMatches pattern' path = do
    annotate (toString (O.renderOPattern pattern') <> "  vs  " <> toString (joinPath path))
    compiled <- compileOrFail pattern'
    assert (isJust (matchTarget compiled (joinPath path)))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

compileOrFail :: (MonadTest m) => OPattern -> m CompiledTargetPattern
compileOrFail pattern' = case compileTargetPattern (O.renderOPattern pattern') of
    Right compiled -> pure compiled
    Left err -> do
        annotate (toString (O.renderOPattern pattern'))
        annotate (toString (renderGlobPlusError err))
        failure

matchOrFail :: (MonadTest m) => CompiledTargetPattern -> [Text] -> m MatchEnv
matchOrFail compiled path = case matchTarget compiled (joinPath path) of
    Just env -> pure env
    Nothing -> failure

matchOf :: (MonadTest m) => OPattern -> [Text] -> m Bool
matchOf pattern' path = do
    compiled <- compileOrFail pattern'
    pure (isJust (matchTarget compiled (joinPath path)))

-- | A globstar inserted at a position that keeps the pattern legal.
forAllLegalInsertion :: OPattern -> PropertyT IO OPattern
forAllLegalInsertion pattern' =
    case [insertAt i OGlobStar pattern' | i <- [0 .. length pattern'], O.isLegalTarget (insertAt i OGlobStar pattern')] of
        [] -> discard
        candidates -> forAll (Gen.element candidates)

joinPath :: [Text] -> Text
joinPath = T.intercalate "/"

expectedTargetDir :: [Text] -> Text
expectedTargetDir path = case nonEmpty path of
    Nothing -> "."
    Just segments -> T.intercalate "/" (init segments)

insertAt :: Int -> a -> [a] -> [a]
insertAt index x xs = take index xs <> [x] <> drop index xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index x xs = take index xs <> [x] <> drop (index + 1) xs

varNameOf :: [Text] -> VarName
varNameOf = VarName . render KebabCase

braced :: Text -> Text
braced t = "{{" <> t <> "}}"

-- | Every variable name occurring in a pattern.
namesOf :: OPattern -> [[Text]]
namesOf pattern' = ordNub [name | OSeg parts <- pattern', OVar name _ <- parts]

-- | Every variable occurrence, as the name and casing it is written in.
occurrencesOf :: OPattern -> [([Text], Casing)]
occurrencesOf pattern' = ordNub [(name, casing) | OSeg parts <- pattern', OVar name casing <- parts]

{- | Every variable occurrence paired with the path index it is pinned to. A
variable with no globstar before it counts from the start; otherwise it has
none after it, and counts from the end.
-}
pinnedOccurrences :: OPattern -> Int -> [(Int, [Text], Casing)]
pinnedOccurrences pattern' pathLength =
    [ (index, name, casing)
    | (position, OSeg parts) <- indexed
    , OVar name casing <- parts
    , index <- toList (pinnedIndex position)
    ]
  where
    indexed = zip [0 :: Int ..] pattern'
    globStarsBefore position = length [() | (i, OGlobStar) <- indexed, i < position]
    globStarsAfter position = length [() | (i, OGlobStar) <- indexed, i > position]

    pinnedIndex position
        | globStarsBefore position == 0 = Just position
        | globStarsAfter position == 0 = Just (pathLength - (length pattern' - position))
        | otherwise = Nothing

capturedValues :: [O.Capture] -> [([Text], Casing, Text)]
capturedValues = L.sort . fmap (\c -> (c.name, c.casing, c.value))

patternChars :: [Char]
patternChars = "abAB/*{}-_.@01"

--------------------------------------------------------------------------------
-- Polarity fixtures
--------------------------------------------------------------------------------

{- | An environment produced the way production produces one: by matching a
target that captures a single variable in a single casing.
-}
genBoundEnv :: Gen (MatchEnv, [Text], Casing)
genBoundEnv = do
    name <- O.genVarName
    casing <- Gen.element [minBound .. maxBound]
    value <- O.genRendering casing name
    let pattern' = "@/probe/" <> braced (render casing name)
    case rightToMaybe (compileTargetPattern pattern') >>= \c -> matchTarget c ("@/probe/" <> value) of
        Just env -> pure (env, name, casing)
        Nothing -> Gen.discard

genSpellingOf :: MatchEnv -> [Text] -> Casing -> Gen Text
genSpellingOf env name casing = case Map.lookup (varNameOf name) env.variables of
    Nothing -> Gen.discard
    Just bound -> Gen.element (spellingsOf casing bound)

-- | Every spelling, in one casing, of every name a capture could have denoted.
spellingsOf :: Casing -> BoundName -> [Text]
spellingsOf casing bound = ordNub (concatMap (toList . renderings casing) bound.candidates)

boundOf :: MatchEnv -> Set VarName
boundOf env = Map.keysSet env.variables

unsafeClause :: Polarity -> Set VarName -> Text -> CompiledClausePattern
unsafeClause polarity bound text = case compileClausePattern polarity bound text of
    Right compiled -> compiled
    Left err -> error ("clause did not compile: " <> renderGlobPlusError err)
