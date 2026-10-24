{- | Every language Deslop speaks, in the order they are tried.

The only module that names a language: supporting one more is a new line in
'supported'. A polyglot project is read as the first language that recognises
it, so the order is a decision, not an accident.
-}
module Frontend (
    supported,
    detectFrontend,
    lookedFor,
) where

import Deslop.Frontend (SomeFrontend, Supported (..), detectFirst, lookingForAny)
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem)
import FileSystem.Path (ProjectRoot)
import Frontend.Haskell (HsFrontend)
import Frontend.Nix (NixFrontend)
import Frontend.TypeScript (TsFrontend)

supported :: [Supported]
supported = [Supported (Proxy @TsFrontend), Supported (Proxy @HsFrontend), Supported (Proxy @NixFrontend)]

detectFrontend :: (RoFileSystem :> es) => ProjectRoot -> Eff es (Maybe SomeFrontend)
detectFrontend = detectFirst supported

lookedFor :: [Text]
lookedFor = lookingForAny supported
