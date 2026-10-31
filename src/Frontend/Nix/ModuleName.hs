{- | What a file of a Nix project, or anything else a path reaches, is called.

A path names a file, so the name is the path from the project root, spelled
with @\/@ (ADR 19), extension and all: @modules\/home\/git.nix@,
@secrets\/common.yaml@. Paths are unique, so canonical names are too (ADR 20).
A @default.nix@ is what a path to its directory imports, so @lib\/default.nix@
is also aliased @lib@: a Rule may say either. See @docs\/adr\/0026@.
-}
module Frontend.Nix.ModuleName (
    moduleNames,
    externalName,
) where

import Data.Text qualified as T
import Deslop.Module (ModuleName, moduleNameUnsafe)
import FileSystem.Path (ProjectRelativePath, portablePath)

-- | Every name a path answers to, canonical first.
moduleNames :: ProjectRelativePath -> NonEmpty ModuleName
moduleNames path = moduleNameUnsafe <$> named :| maybeToList (T.stripSuffix "/default.nix" named)
  where
    named = portablePath path

{- | A module outside the project by its segments, named the way a project's
own is: @\<nixpkgs\/lib\/default.nix\>@ is @nixpkgs\/lib\/default.nix@.
-}
externalName :: NonEmpty Text -> ModuleName
externalName = moduleNameUnsafe . T.intercalate "/" . toList
