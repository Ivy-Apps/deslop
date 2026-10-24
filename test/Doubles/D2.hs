{- | A d2 that is either installed or not, standing in for the real program so
no test needs it on @PATH@.

The installed one renders nothing, but answers as d2 would and writes the
@index.svg@ it would have written, so a test can tell a rendered diagram from
one that never was.
-}
module Doubles.D2 (
    FakeD2 (..),
    runFakeD2,
) where

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects.D2 (Availability (..), D2 (..), Rendering (..))
import Effects.FileSystem (WrFileSystem, fsMkDirP, fsWriteFile)
import FileSystem.Path (AbsPath (osPath), absPathUnsafe, withAbsBaseUnsafe)
import System.OsPath (dropExtension, osp)

data FakeD2 = D2Installed | D2Missing
    deriving stock (Show, Eq)

runFakeD2 :: (WrFileSystem :> es) => FakeD2 -> Eff (D2 : es) a -> Eff es a
runFakeD2 fake = interpret $ \_ -> \case
    CheckAvailability -> pure $ case fake of
        D2Installed -> Installed
        D2Missing -> NotInstalled
    Render _source output -> case fake of
        D2Installed -> do
            let dir = absPathUnsafe . dropExtension $ output.osPath
            fsMkDirP dir
            fsWriteFile (withAbsBaseUnsafe dir [osp|index.svg|]) "<svg/>"
            pure Rendered
        D2Missing -> error "runFakeD2: rendered without d2 installed"
