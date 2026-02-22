module Effects.ReportProblem where

import Data.Text (Text)
import Effectful (Effect)

data Location = Location
    { file :: FilePath
    , code :: Text
    }
newtype ProblemId = ProblemId Text

data Severity = Error

data Problem = Problem
    { id :: ProblemId
    , location :: Location
    , severity :: Severity
    , description :: Text
    , fix :: Text
    }

data ReportProblem :: Effect where
  Report :: Problem -> ReportProblem m ()
