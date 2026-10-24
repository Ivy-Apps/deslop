{- | What a Nix project's references are read against: the names its root
goes by, and the inputs its flake declares.

@self@ always names the root. A project that passes it on under another
name - @specialArgs = { root = self; }@ - says so in @deslop\/nix.yaml@:

> roots: [root]

The file is optional; one Deslop cannot read stops the run, since every
reference read without it would be read wrong.
-}
module Frontend.Nix.Config (
    NixConfigError (..),
    loadNixConfig,
    settingsPath,
) where

import Data.Aeson (FromJSON (..), Options (..), defaultOptions, genericParseJSON)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Yaml (decodeEither', prettyPrintParseException)
import Deslop.Error (Warning (..))
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsReadFileIfExists)
import FileSystem.Path (ProjectRelativePath (..), ProjectRoot (..), encodeOsPath, portablePath, relativePathUnsafe, withAbsBaseUnsafe)
import Frontend.Nix.FlakeInputs (declaredInputs)
import Frontend.Nix.Lexer (isIdentifier, lexNix)
import Frontend.Nix.Reference (Scope (..))
import Renderable (Renderable (..))

data NixConfigError
    = -- | Not YAML, or not the keys Deslop knows.
      SettingsUnreadable Text
    | -- | A root Nix could never bind, such as @my root@.
      RootNotAnIdentifier Text
    deriving stock (Show, Eq)

instance Renderable NixConfigError where
    render = \case
        SettingsUnreadable reason -> "Failed to read " <> settings <> ":\n" <> reason
        RootNotAnIdentifier name ->
            settings <> " names the root \"" <> name <> "\", which is not a Nix identifier; list the names the project root is bound to, such as root."
      where
        settings = portablePath settingsPath

newtype SettingsDto = SettingsDto
    { roots :: Maybe [Text]
    }
    deriving stock (Generic)

instance FromJSON SettingsDto where
    parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Where the settings live, from the project root.
settingsPath :: ProjectRelativePath
settingsPath = relativePathUnsafe (encodeOsPath "deslop/nix.yaml")

loadNixConfig :: (RoFileSystem :> es) => ProjectRoot -> Eff es (Either NixConfigError (Scope, [Warning]))
loadNixConfig projectRoot = do
    settings <- optionalFile settingsPath
    flake <- optionalFile (relativePathUnsafe (encodeOsPath "flake.nix"))
    pure $ do
        roots <- maybe (Right []) rootsIn settings
        let (inputs, warnings) = maybe (mempty, []) inputsOf flake
        Right (Scope {roots = Set.fromList ("self" : roots), inputs}, warnings)
  where
    optionalFile rel = fsReadFileIfExists (withAbsBaseUnsafe projectRoot.path rel.osPath)

rootsIn :: ByteString -> Either NixConfigError [Text]
rootsIn bytes = do
    dto <- first (SettingsUnreadable . toText . prettyPrintParseException) (decodeEither' @SettingsDto bytes)
    traverse identifier (fromMaybe [] dto.roots)
  where
    identifier name
        | isIdentifier name = Right name
        | otherwise = Left (RootNotAnIdentifier name)

-- | Its inputs, or none and a warning when it cannot be read.
inputsOf :: ByteString -> (Set Text, [Warning])
inputsOf bytes = case lexNix =<< rightToMaybe (TE.decodeUtf8' bytes) of
    Just lexemes -> (declaredInputs lexemes, [])
    Nothing -> (mempty, [Warning "flake.nix could not be read, so no reference is read as a flake input."])
