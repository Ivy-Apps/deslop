{- | Names, and the four ways a codebase spells them.

A /name/ is a list of lower-case words. A /casing/ is a way of writing one
down. kebab-case and CONSTANT_CASE mark word boundaries with a separator, so
reading them back is exact. PascalCase and camelCase mark boundaries with a
capital, which a run of capitals destroys: @HTTPClient@ is a perfectly ordinary
spelling of @["http","client"]@, but so is @HttpClient@, and nothing in the
string says which words were acronyms.

The way out is to never ask what a spelling /decodes to/, and instead ask
whether two spellings /could denote the same name/. 'spells' answers that
exactly, and it is what 'agree' is built on. Decoding is still needed when a
name has to be written out in a casing it was never captured in, and 'decode'
does it by reading each run of capitals as one word - the reading a human
intends often enough to be worth it, and wrong for two adjacent acronyms.
-}
module Deslop.Casing (
    -- * Casings
    Casing (..),
    casingName,
    spelledIn,

    -- * Names
    AgreedName (..),
    agree,
    spells,
    decode,
    decodings,
    render,
    renderings,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

{- | How a name is spelled. A name is written in exactly one casing at each
occurrence, and the spelling alone determines which one - there is no separate
annotation.
-}
data Casing
    = -- | @ProviderName@
      PascalCase
    | -- | @providerName@
      CamelCase
    | -- | @PROVIDER_NAME@
      ConstantCase
    | -- | @provider-name@
      KebabCase
    deriving (Show, Eq, Ord, Enum, Bounded)

casingName :: Casing -> Text
casingName PascalCase = "PascalCase"
casingName CamelCase = "camelCase"
casingName KebabCase = "kebab-case"
casingName ConstantCase = "CONSTANT_CASE"

-- | Whether a token is a well-formed spelling in the given casing.
spelledIn :: Casing -> Text -> Bool
spelledIn PascalCase = startsWith isAsciiUpper isAsciiAlphaNum
spelledIn CamelCase = startsWith isAsciiLower isAsciiAlphaNum
spelledIn KebabCase = separatedBy '-' (\c -> isAsciiLower c || isDigit c)
spelledIn ConstantCase = separatedBy '_' (\c -> isAsciiUpper c || isDigit c)

--------------------------------------------------------------------------------
-- Agreement
--------------------------------------------------------------------------------

{- | The name a group of occurrences settled on, and every name they could
equally have denoted. 'canonical' is the coarsest of 'candidates' and is what
gets written out; 'candidates' is what a clause widens over when a missed match
would be worse than a spurious one.
-}
data AgreedName = AgreedName
    { canonical :: [Text]
    , candidates :: NonEmpty [Text]
    }
    deriving (Show, Eq)

{- | The name every occurrence spells, if there is one.

A candidate only has to be proposed by /some/ occurrence, because a candidate
no occurrence proposed cannot survive the check anyway. kebab-case and
CONSTANT_CASE each propose exactly one name and it is exact, so whenever the
group contains one of them the answer is decided by membership alone.
-}
agree :: NonEmpty (Casing, Text) -> Maybe AgreedName
agree occurrences = do
    survivors <- nonEmpty (filter spelledByEvery proposed)
    pure
        AgreedName
            { canonical = NE.head (NE.sortWith length survivors)
            , candidates = survivors
            }
  where
    proposed = ordNub (concatMap (toList . uncurry decodings) occurrences)
    spelledByEvery name = all (\(casing, text) -> spells casing name text) occurrences

{- | Whether a name, written in a casing, could have produced this text.

Each word of a Pascal or camel spelling is written either capitalised or
wholly upper-case, so the check walks the text word by word and tries both.
It is exact where 'decode' guesses: @["api","2fa"]@ spells @Api2fa@, which no
amount of splitting on capitals could ever recover.
-}
spells :: Casing -> [Text] -> Text -> Bool
spells KebabCase name text = text == render KebabCase name
spells ConstantCase name text = text == render ConstantCase name
spells PascalCase name text = spellsCapitalised name text
spells CamelCase name text = case name of
    [] -> T.null text
    (first' : rest) -> maybe False (spellsCapitalised rest) (T.stripPrefix first' text)

-- | Whether each word appears in order, capitalised or wholly upper-case.
spellsCapitalised :: [Text] -> Text -> Bool
spellsCapitalised [] text = T.null text
spellsCapitalised (word : rest) text =
    any consumes (ordNub [capitalise word, T.toUpper word])
  where
    consumes spelling = maybe False (spellsCapitalised rest) (T.stripPrefix spelling text)

--------------------------------------------------------------------------------
-- Decoding
--------------------------------------------------------------------------------

{- | The name a spelling most likely denotes, reading each run of capitals as
one word. Exact for kebab-case and CONSTANT_CASE. For Pascal and camel it is a
guess, and the guess is wrong for two adjacent acronyms (@AWSS3Client@ reads as
@aws s3 client@ to a human and as @awss3 client@ here) and for single-letter
words (@ABTest@ reads as @ab test@).
-}
decode :: Casing -> Text -> [Text]
decode KebabCase = splitOnSeparator '-'
decode ConstantCase = fmap T.toLower . splitOnSeparator '_'
decode PascalCase = coarsestGrouping isCapitalisedBlock . atoms
decode CamelCase = coarsestGrouping isLeadingBlock . atoms

{- | Every name a spelling could denote. A separator-bearing casing proposes
exactly one; Pascal and camel propose one per way of grouping their atoms.
-}
decodings :: Casing -> Text -> NonEmpty [Text]
decodings KebabCase text = one (decode KebabCase text)
decodings ConstantCase text = one (decode ConstantCase text)
decodings PascalCase text = groupingsOf isCapitalisedBlock (atoms text)
decodings CamelCase text = groupingsOf isLeadingBlock (atoms text)

{- | Every grouping of a spelling's atoms into words, coarsest first.

A group is kept only when what it spells is a word: @HTTP@ and @Http@ and
@Http2@ each are, @HttpClient@ is not. Because a group that is not a word
cannot become one by growing, the search stops extending as soon as it fails.
-}
groupingsOf :: (Text -> Bool) -> [Text] -> NonEmpty [Text]
groupingsOf isFirstBlock as = case nonEmpty (take candidateLimit (go isFirstBlock as)) of
    Just groupings -> groupings
    Nothing -> one (coarsestGrouping isFirstBlock as)
  where
    go _ [] = [[]]
    go isBlockHere remaining =
        [ T.toLower block : rest
        | n <- reverse [1 .. length remaining]
        , let block = T.concat (take n remaining)
        , isBlockHere block
        , rest <- go isCapitalisedBlock (drop n remaining)
        ]

{- | Above this, only the coarsest alternatives are kept. A name with more than
a handful of acronym letters is pathological rather than real, and the reading
'decode' returns comes first, so it is never the one dropped.
-}
candidateLimit :: Int
candidateLimit = 64

{- | The coarsest grouping: the longest word that can be read off, repeatedly.
Growing a group never rescues one that is already not a word, so taking the
longest each time is also the fewest overall.
-}
coarsestGrouping :: (Text -> Bool) -> [Text] -> [Text]
coarsestGrouping _ [] = []
coarsestGrouping isFirstBlock as = case longest of
    Just (n, block) -> T.toLower block : coarsestGrouping isCapitalisedBlock (drop n as)
    -- Not reachable through a capture regex, but decoding stays total.
    Nothing -> T.toLower <$> as
  where
    longest =
        viaNonEmpty head
            [ (n, block)
            | n <- reverse [1 .. length as]
            , let block = T.concat (take n as)
            , isFirstBlock block
            ]

{- | @Client@, @HTTP@ or @2fa@: a word capitalised, or a word shouted.
Capitalising a word that opens with a digit changes nothing, so a block may
open with one - which is the only trace @["api","2fa"]@ leaves in @Api2fa@.
-}
isCapitalisedBlock :: Text -> Bool
isCapitalisedBlock block =
    startsWith (\c -> isAsciiUpper c || isDigit c) isWordTail block
        || isShoutedBlock block

-- | @http@ in @httpClient@: only a camel spelling's first word looks like this.
isLeadingBlock :: Text -> Bool
isLeadingBlock block = startsWith isAsciiLower isWordTail block || isCapitalisedBlock block

isShoutedBlock :: Text -> Bool
isShoutedBlock block = not (T.null block) && T.all (\c -> isAsciiUpper c || isDigit c) block

isWordTail :: Char -> Bool
isWordTail c = isAsciiLower c || isDigit c

{- | Splits a spelling at every point a word boundary could fall: before a
capital, which marks one, and before a run of digits, which hides one -
@Api2fa@ is how @["api","2fa"]@ comes out, since capitalising @2fa@ changes
nothing.
-}
atoms :: Text -> [Text]
atoms = reverse . fmap T.reverse . T.foldl' step []
  where
    step [] c = [T.singleton c]
    step (current : done) c
        -- 'current' is reversed, so its head is the preceding character.
        | opensAtom (T.head current) c = T.singleton c : current : done
        | otherwise = T.cons c current : done

    opensAtom preceding c = isAsciiUpper c || (isDigit c && not (isDigit preceding))

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Writes a name out in a casing, capitalising rather than upper-casing.
render :: Casing -> [Text] -> Text
render PascalCase = T.concat . fmap capitalise
render CamelCase = \case
    [] -> ""
    (first' : rest) -> first' <> T.concat (capitalise <$> rest)
render KebabCase = T.intercalate "-"
render ConstantCase = T.intercalate "_" . fmap T.toUpper

{- | Every spelling of a name in a casing, canonical first. Pascal and camel
may write any word wholly upper-case, so an acronym-bearing name has several.
-}
renderings :: Casing -> [Text] -> NonEmpty Text
renderings KebabCase name = one (render KebabCase name)
renderings ConstantCase name = one (render ConstantCase name)
renderings PascalCase name = T.concat <$> wordSpellings name
renderings CamelCase name = case name of
    [] -> one ""
    (first' : rest) -> (first' <>) . T.concat <$> wordSpellings rest

wordSpellings :: [Text] -> NonEmpty [Text]
wordSpellings = traverse spellingsOf
  where
    spellingsOf word = case ordNub [capitalise word, T.toUpper word] of
        (canonical' : alternatives) -> canonical' :| alternatives
        [] -> one word

--------------------------------------------------------------------------------
-- Text helpers
--------------------------------------------------------------------------------

splitOnSeparator :: Char -> Text -> [Text]
splitOnSeparator separator = filter (not . T.null) . T.splitOn (T.singleton separator)

capitalise :: Text -> Text
capitalise text = case T.uncons text of
    Nothing -> ""
    Just (c, cs) -> T.toUpper (T.singleton c) <> cs

startsWith :: (Char -> Bool) -> (Char -> Bool) -> Text -> Bool
startsWith isFirst isRest text = case T.uncons text of
    Just (c, rest) -> isFirst c && T.all isRest rest
    Nothing -> False

separatedBy :: Char -> (Char -> Bool) -> Text -> Bool
separatedBy separator isBody text =
    all (\segment -> not (T.null segment) && T.all isBody segment) $
        T.splitOn (T.singleton separator) text

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c
