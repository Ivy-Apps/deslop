{- | Files compiled into the executable, so it needs nothing beside it at run
time.

A path is read relative to the package root, which is where cabal runs GHC,
and must be listed under @extra-source-files@ so a source distribution - and
so a Nix build - carries it. Editing the file recompiles the splice.
-}
module FileSystem.Embed (embedText) where

import Language.Haskell.TH (Code, Q, bindCode)
import Language.Haskell.TH.Syntax (addDependentFile, liftTyped, runIO)

-- | A UTF-8 file's contents, as they were when the splice was compiled.
embedText :: FilePath -> Code Q Text
embedText path = bindCode (readUtf8 path) liftTyped
  where
    readUtf8 p = do
        addDependentFile p
        decodeUtf8 <$> runIO (readFileBS p)
