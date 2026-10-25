{- | A desktop held in memory: whether a person sits at it, and what its
browser does with a page. Every page it is asked to open is recorded, since
opening one is the behaviour under test.
-}
module Doubles.Browser (
    Desktop (..),
    runBrowserInMemory,
    unattended,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects.Browser (Attendance (..), Browser (..), LaunchFailure)
import FileSystem.Path (AbsPath)

data Desktop = Desktop
    { attendance :: Attendance
    , launch :: Either LaunchFailure ()
    -- ^ What opening any page answers.
    , openedRef :: Maybe (IORef [AbsPath])
    -- ^ Where, if anywhere, the pages asked to open are collected, in order.
    }

runBrowserInMemory :: (IOE :> es) => Desktop -> Eff (Browser : es) a -> Eff es a
runBrowserInMemory desktop = interpret $ \_ -> \case
    AttendanceOf -> pure desktop.attendance
    Open page -> do
        whenJust desktop.openedRef $ \ref -> liftIO . modifyIORef' ref $ (<> [page])
        pure desktop.launch

-- | A desktop nobody sits at, as in CI: no page is ever opened.
unattended :: Desktop
unattended = Desktop {attendance = Unattended, launch = Right (), openedRef = Nothing}
