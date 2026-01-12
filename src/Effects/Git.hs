module Effects.Git where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Process (readProcess)

data Git :: Effect where
    ModifiedFiles :: Git m [FilePath]

type instance DispatchOf Git = Dynamic

modifiedFiles :: (Git :> es) => Eff es [FilePath]
modifiedFiles = send $ ModifiedFiles

runGit :: (IOE :> es) => Eff (Git : es) a -> Eff es a
runGit = interpret $ \_ -> \case
    ModifiedFiles -> liftIO gitModifiedFiles

gitModifiedFiles :: IO [FilePath]
gitModifiedFiles =
    lines
        <$> readProcess
            "git"
            [ "diff"
            , "--name-only"
            , "HEAD"
            ]
            ""