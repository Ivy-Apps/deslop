{- | What a Nix file, or anything else a path reaches, is called.

A path names a file, so the name is the path from the project root, spelled
with @\/@ (ADR 19): @modules\/home\/git.nix@ is @modules\/home\/git@. What is
not Nix keeps its extension, @secrets\/common.yaml@, since nothing else tells
it apart from the Nix file beside it. A @default.nix@ is what a path to its
directory imports, so @lib\/default.nix@ is canonically @lib\/default@ and
aliased @lib@ (ADR 20): the canonical name stays unique even beside a
@lib.nix@, and a Rule may still say @lib@.

What is not Nix can still be spelled as a Nix file's name: the directory
@pkg@ a @src = .\/pkg@ reaches beside @pkg.nix@, or a script @deploy@ beside
@deploy.nix@. Such a module is canonically @.\/pkg@, as a Nix file would
write it from the root, and aliased @pkg@. No path from the root has a @.@
segment, so no @.nix@ module is ever called that.
-}
module Frontend.Nix.ModuleName (
    projectNames,
    moduleNames,
    externalName,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Module (ModuleName (..), moduleNameUnsafe)
import FileSystem.Path (ProjectRelativePath, portablePath)

{- | Every module's names, canonical first, given the path of every module of
the project: the canonical names are unique among them.
-}
projectNames :: [ProjectRelativePath] -> Map ProjectRelativePath (NonEmpty ModuleName)
projectNames paths = Map.fromList [(path, distinct path (moduleNames path)) | path <- paths]
  where
    nixNames = Set.fromList [head (moduleNames path) | path <- paths, isJust (nixStem path)]
    distinct path names = case (nixStem path, head names `Set.member` nixNames) of
        (Nothing, True) -> NE.cons (fromRoot (head names)) names
        _ -> names
    fromRoot name = moduleNameUnsafe ("./" <> name.text)

{- | What one path is called on its own, canonical first; 'projectNames' tells
it apart from the @.nix@ file beside it.
-}
moduleNames :: ProjectRelativePath -> NonEmpty ModuleName
moduleNames path = case nixStem path of
    Just named -> case T.stripSuffix "/default" named of
        Just directory -> moduleNameUnsafe named :| [moduleNameUnsafe directory]
        Nothing -> moduleNameUnsafe named :| []
    Nothing -> moduleNameUnsafe (portablePath path) :| []

-- | A @.nix@ file's path without the extension.
nixStem :: ProjectRelativePath -> Maybe Text
nixStem = T.stripSuffix ".nix" . portablePath

{- | A module outside the project by its segments, named the way a project's
own is: @\<nixpkgs\/lib\/default.nix\>@ is @nixpkgs\/lib\/default@.
-}
externalName :: NonEmpty Text -> ModuleName
externalName = moduleNameUnsafe . (\name -> fromMaybe name (T.stripSuffix ".nix" name)) . T.intercalate "/" . toList
