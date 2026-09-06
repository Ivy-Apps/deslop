{-# LANGUAGE QuasiQuotes #-}

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
    dropCommonSegments,
    portablePath,

    -- * The project under inspection
    ProjectRoot (..),
) where

import Control.Monad.Catch.Pure (runCatch)
import Data.Text qualified as T
import System.OsPath (OsPath, decodeUtf, encodeUtf, joinPath, osp, splitDirectories, (</>))

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

{- | Where @target@ sits, spelled from @root@.

Goes back out of the root with @..@ where it has to, rather than
'System.OsPath.makeRelative', which hands the target back unchanged whenever
the root is not a prefix of it. Every reported path and therefore every Problem
Id is built from this, and a Baseline is committed and read back on another
machine and in CI, so an answer that quietly stayed absolute would name the
machine that wrote it and match nothing anywhere else.

The one target it cannot answer for is one sharing no segment at all with the
root - a different Windows drive, and nothing else - where no relative path
exists and the target is returned as it stands.
-}
relativePathTo :: ProjectRoot -> AbsPath -> RelativePath
relativePathTo (ProjectRoot (AbsPath base)) (AbsPath target) =
    case (splitDirectories base, splitDirectories target) of
        (b : baseSegs, t : targetSegs)
            | b == t -> spelledFrom $ dropCommonSegments baseSegs targetSegs
        _ -> RelativePath target
  where
    spelledFrom (baseRest, targetRest) =
        RelativePath . spell $ replicate (length baseRest) [osp|..|] <> targetRest

    -- joinPath of nothing is "", which names no file; the root itself is ".".
    spell [] = [osp|.|]
    spell segs = joinPath segs

{- | A path spelled with @/@, whatever separator this OS writes.

Paths travel: into Baselines users commit and share across machines, into
goldens, and into @deslop fix@'s skip-list. A native decode of a Windows
'RelativePath' yields backslashes, which would make every id this run produces
unmatchable against a Baseline written on any other OS - silently
unsuppressing problems and un-fixing imports that a teammate had already
accepted.
-}
portablePath :: RelativePath -> Text
portablePath = T.replace "\\" "/" . decodeOsPath . (.osPath)

{- | The two paths with their shared leading segments removed, left as the part
of each that the other does not have.
-}
dropCommonSegments :: (Eq a) => [a] -> [a] -> ([a], [a])
dropCommonSegments (x : xs) (y : ys) | x == y = dropCommonSegments xs ys
dropCommonSegments xs ys = (xs, ys)

{- | The directory a run reports paths relative to.

Named rather than passed as a bare 'AbsPath' because it travels through a
@Reader@, where a second 'AbsPath' in scope would be indistinguishable from it.
-}
newtype ProjectRoot = ProjectRoot
    { path :: AbsPath
    }
    deriving (Show, Eq)
