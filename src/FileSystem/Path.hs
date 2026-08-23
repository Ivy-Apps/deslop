{- | The path vocabulary, and the encoding between 'OsPath' and 'Text'.

Nominal rather than a bare 'OsPath': a path that has been made absolute and a
path that is relative to something are different things, and only one of them
can be opened. Nothing here performs IO - reaching the filesystem is
"Effects.FileSystem" - so a pure module may name a path without taking on an
effect to do it.
-}
module FileSystem.Path (
    -- * Encoding
    encodeOsPath,
    encodeOsPathString,
    decodeOsPath,

    -- * Absolute paths
    AbsPath (osPath),
    absPathUnsafe,
    withAbsBaseUnsafe,
    withAbsBaseSafe,

    -- * Relative paths
    RelativePath (osPath),
    relativePathUnsafe,
    relativePathTo,

    -- * The project under inspection
    ProjectRoot (..),
) where

import Control.Monad.Catch.Pure (runCatch)
import Data.Text qualified as T
import System.OsPath (OsPath, decodeUtf, encodeUtf, makeRelative, (</>))

encodeOsPath :: Text -> OsPath
encodeOsPath = encodeOsPathString . T.unpack

encodeOsPathString :: FilePath -> OsPath
encodeOsPathString p =
    case runCatch (encodeUtf p) of
        Right path -> path
        Left err -> error $ "encodeOsPath failed: " <> show err

decodeOsPath :: OsPath -> Text
decodeOsPath = either handleErr T.pack . runCatch . decodeUtf
  where
    handleErr err = error $ "decodeOsPath failed: " <> show err

newtype AbsPath = AbsPath
    { osPath :: OsPath
    }
    deriving (Show, Eq, Ord)

absPathUnsafe :: OsPath -> AbsPath
absPathUnsafe = AbsPath

withAbsBaseUnsafe :: AbsPath -> OsPath -> AbsPath
withAbsBaseUnsafe (AbsPath b) p = AbsPath (b </> p)

withAbsBaseSafe :: AbsPath -> OsPath -> OsPath
withAbsBaseSafe (AbsPath b) p = b </> p

newtype RelativePath = RelativePath
    { osPath :: OsPath
    }
    deriving (Show, Eq, Ord)

relativePathUnsafe :: OsPath -> RelativePath
relativePathUnsafe = RelativePath

relativePathTo :: AbsPath -> AbsPath -> RelativePath
relativePathTo (AbsPath base) (AbsPath target) = RelativePath $ makeRelative base target

{- | The directory a run reports paths relative to.

Named rather than passed as a bare 'AbsPath' because it travels through a
@Reader@, where a second 'AbsPath' in scope would be indistinguishable from it.
-}
newtype ProjectRoot = ProjectRoot
    { path :: AbsPath
    }
    deriving (Show, Eq)
