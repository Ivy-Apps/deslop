{- | Reaching the @d2@ diagram renderer installed on this machine.

Deslop writes the @.d2@ source itself; laying it out and drawing it as SVG is
d2's job, so it runs as a separate program found on @PATH@.
-}
module Effects.D2 (
    D2 (..),
    Availability (..),
    Rendering (..),
    d2Availability,
    d2Render,
    runD2IO,
    runD2ProgramIO,
) where

import Control.Exception (IOException, try)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import FileSystem.Path (AbsPath (osPath), decodeOsPath)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data Availability = Installed | NotInstalled
    deriving stock (Show, Eq)

data Rendering
    = Rendered
    | -- | What d2 said on stderr about why it could not.
      RenderFailed Text
    deriving stock (Show, Eq)

data D2 :: Effect where
    CheckAvailability :: D2 m Availability
    {- | Renders a @.d2@ source to an SVG. A diagram with layers is written as
    a directory named after the SVG instead, holding @index.svg@ and one
    SVG per layer.
    -}
    Render :: AbsPath -> AbsPath -> D2 m Rendering

type instance DispatchOf D2 = Dynamic

d2Availability :: (D2 :> es) => Eff es Availability
d2Availability = send CheckAvailability

d2Render :: (D2 :> es) => AbsPath -> AbsPath -> Eff es Rendering
d2Render source output = send $ Render source output

runD2IO :: (IOE :> es) => Eff (D2 : es) a -> Eff es a
runD2IO = runD2ProgramIO "d2"

{- | Interprets 'D2' by running the given program as d2: a name found on
@PATH@, or a path to it.
-}
runD2ProgramIO :: (IOE :> es) => FilePath -> Eff (D2 : es) a -> Eff es a
runD2ProgramIO program = interpret $ \_env -> \case
    CheckAvailability -> liftIO $ availability <$> runD2 ["--version"]
    Render source output ->
        liftIO . fmap rendering . runD2 $ renderFlags <> [filePath source, filePath output]
  where
    availability (Right (ExitSuccess, _)) = Installed
    availability _notRunnable = NotInstalled

    rendering (Right (ExitSuccess, _)) = Rendered
    rendering (Right (ExitFailure _, said)) = RenderFailed . T.strip $ toText said
    rendering (Left err) = RenderFailed $ show err

    filePath :: AbsPath -> String
    filePath = toString . decodeOsPath . (.osPath)

    -- Answers d2's exit code and stderr, or why it could not be started at all.
    runD2 :: [String] -> IO (Either IOException (ExitCode, String))
    runD2 args = try $ (\(code, _out, err) -> (code, err)) <$> readProcessWithExitCode program args ""

{- | ELK lays out nested containers far better than d2's default engine. The
generous timeout is for ELK on a large board, which can take minutes.
-}
renderFlags :: [String]
renderFlags =
    [ "--layout"
    , "elk"
    , "--theme"
    , "0"
    , "--dark-theme"
    , "200"
    , "--pad"
    , "40"
    , "--omit-version"
    , "--timeout"
    , "600"
    ]
