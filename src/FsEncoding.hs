{-# LANGUAGE Unsafe #-}

{- | Filesystem encoding at boundaries (CLI, logs, golden output). Uses @encodeFS@ / @decodeFS@
from @os-string@ so behavior matches @base@ I/O.
-}
module FsEncoding (
    readOsPathArg,
    decodePathString,
    encodePathString,
) where

import Control.Exception (SomeException, displayException, try)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, decodeFS, encodeFS)

{-# NOINLINE readOsPathArg #-}
readOsPathArg :: String -> Either String OsPath
readOsPathArg s = unsafePerformIO $ do
    e <- try @SomeException (encodeFS s)
    pure $ case e of
        Left ex -> Left (displayException ex)
        Right p -> Right p

{-# NOINLINE decodePathString #-}
decodePathString :: OsPath -> String
decodePathString p = unsafePerformIO (decodeFS p)

{-# NOINLINE encodePathString #-}
encodePathString :: String -> OsPath
encodePathString s = case readOsPathArg s of
    Left err -> error ("encodePathString: " ++ err)
    Right p -> p
