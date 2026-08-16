{- | The saved measurements a run is judged against.

Called the Reference and never the Baseline: a Baseline in this project is the
set of accepted Problem IDs in @deslop/baseline.yaml@, and @baseline@ is also
one of the groups this benchmark measures. Three meanings of one word, one of
them printed inside this tool's own output, is one too many.
-}
module Bench.Reference (
    Reference (..),
    RecordedCase (..),
    Environment (..),
    ReferenceState (..),
    referencePath,
    currentEnvironment,
    loadReference,
    saveReference,
    lookupCase,
    recordOf,
) where

import Bench.Fixtures (Case (..), Fixture (..), commandName)
import Bench.Measurement (Measurement)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Version (showVersion)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import System.Directory (doesFileExist)
import System.Info (arch, compilerVersion, os)

-- | Where the Reference lives, relative to the project root.
referencePath :: FilePath
referencePath = "bench/reference.yaml"

{- | Whether a Reference exists to compare against.

A missing file is an ordinary state, not a failure: it is what a first run
looks like.
-}
data ReferenceState
    = Unrecorded
    | Recorded Reference
    deriving stock (Show, Eq)

data Reference = Reference
    { environment :: Environment
    , recorded :: UTCTime
    , cases :: [RecordedCase]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RecordedCase = RecordedCase
    { fixture :: Text
    , command :: Text
    , measurement :: Measurement
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

{- | The conditions a set of measurements was taken under.

Recorded so that a run can say so when the numbers it is about to compare came
from a different compiler, machine or capability count. Without it a 30% shift
from a GHC upgrade is indistinguishable from a 30% regression in Deslop.
-}
data Environment = Environment
    { ghc :: Text
    , os :: Text
    , arch :: Text
    , processors :: Int
    , capabilities :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

currentEnvironment :: IO Environment
currentEnvironment = do
    processors <- getNumProcessors
    capabilities <- getNumCapabilities
    pure
        Environment
            { ghc = T.pack . showVersion $ compilerVersion
            , os = T.pack os
            , arch = T.pack arch
            , processors = processors
            , capabilities = capabilities
            }

-- | Reads the Reference. A @Left@ means the file is there but unusable.
loadReference :: IO (Either Text ReferenceState)
loadReference = do
    exists <- doesFileExist referencePath
    if exists
        then fmap (bimap renderErr Recorded) . decodeFileEither $ referencePath
        else pure . Right $ Unrecorded
  where
    renderErr e =
        toText referencePath
            <> " could not be read:\n"
            <> toText (prettyPrintParseException e)

{- | Writes the Reference with its keys sorted, so that re-recording produces a
diff of the numbers that changed rather than of the whole file.
-}
saveReference :: Reference -> IO ()
saveReference = BS.writeFile referencePath . encodePretty conf
  where
    conf = setConfCompare compare defConfig

lookupCase :: Reference -> Case -> Maybe Measurement
lookupCase reference c =
    fmap (.measurement)
        . find (\r -> r.fixture == c.fixture.name && r.command == commandName c.command)
        $ reference.cases

recordOf :: Case -> Measurement -> RecordedCase
recordOf c m =
    RecordedCase
        { fixture = c.fixture.name
        , command = commandName c.command
        , measurement = m
        }
