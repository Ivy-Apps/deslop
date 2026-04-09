module Effects.Git (
    Git (..),
    modifiedFiles,
    runGit,
    gitModifiedFiles,
) where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import FsEncoding (readOsPathArg)
import System.OsPath (OsPath)
import System.Process (readProcess)

data Git :: Effect where
    ModifiedFiles :: Git m [OsPath]

type instance DispatchOf Git = Dynamic

modifiedFiles :: (Git :> es) => Eff es [OsPath]
modifiedFiles = send ModifiedFiles

runGit :: (IOE :> es) => Eff (Git : es) a -> Eff es a
runGit = interpret $ \_ -> \case
    ModifiedFiles -> liftIO gitModifiedFiles

gitModifiedFiles :: IO [OsPath]
gitModifiedFiles = do
    out <-
        readProcess
            "git"
            [ "diff"
            , "--name-only"
            , "origin/main"
            ]
            ""
    let cleanLines = filter (/= "") . lines . toText $ out
    either fail pure $ traverse (readOsPathArg . toString) cleanLines
