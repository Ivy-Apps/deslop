{- | What @hspec-discover@ generates for a test driver, so the driver's
dependencies are known without running it.

A driver is a file whose header asks for @{-# OPTIONS_GHC -F -pgmF
hspec-discover #-}@. GHC compiles the module @hspec-discover@ writes in its
place, which imports every spec below the driver's directory: every file whose
name ends in @Spec.hs@ and whose path, from that directory, is a valid module
name - @Deslop\/ModuleSpec.hs@ is @Deslop.ModuleSpec@, and @Spec.hs@ itself,
whose name before @Spec@ is empty, is none.
-}
module Frontend.Haskell.HspecDiscover (
    DiscoveredSpec (..),
    discoveredSpecs,
) where

import Data.List qualified as L
import Data.Text qualified as T
import FileSystem.Path (AbsPath (..), decodeOsPath)
import Frontend.Haskell.ModuleName (HsModuleName, fromSegments)
import System.OsPath (splitDirectories, takeDirectory)

data DiscoveredSpec = DiscoveredSpec
    { path :: AbsPath
    , name :: HsModuleName
    -- ^ As @hspec-discover@ writes it in the import it generates.
    }
    deriving stock (Show, Eq)

-- | The specs a driver imports, from among the files given.
discoveredSpecs :: AbsPath -> [AbsPath] -> [DiscoveredSpec]
discoveredSpecs driver files =
    [ DiscoveredSpec file name
    | file <- files
    , file /= driver
    , Just below <- [L.stripPrefix driverDir (splitDirectories file.osPath)]
    , Just name <- [specName (decodeOsPath <$> below)]
    ]
  where
    driverDir = splitDirectories (takeDirectory driver.osPath)

specName :: [Text] -> Maybe HsModuleName
specName segments = do
    (dirs, file) <- unsnoc segments
    stem <- T.stripSuffix "Spec.hs" file
    guard (not (T.null stem))
    fromSegments (dirs <> [stem <> "Spec"])
  where
    unsnoc xs = (,) <$> viaNonEmpty init xs <*> viaNonEmpty last xs
