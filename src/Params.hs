{-# LANGUAGE QuasiQuotes #-}

module Params (
    pParams,
    paramsParser,
    parserPrefs,
    Params (..),
    ParamsDto (..),
    Command (..),
    Exploration (..),
    Opening (..),
    paramsFromDto,
)
where

import Data.Text qualified as T
import Data.Version (showVersion)
import Deslop.Command (Command (..), Exploration (..), Opening (..))
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import Effects.Git (GitRef (..))
import FileSystem.Path (AbsPath, encodeOsPath)
import Options.Applicative
import Paths_deslop (version)
import System.OsPath (OsPath, osp)

data Params = Params
    { command :: Command
    , projectPath :: AbsPath
    }
    deriving (Show, Eq)

data ParamsDto = ParamsDto
    { command :: Command
    , projectPath :: OsPath
    }
    deriving (Show, Eq)

paramsFromDto :: (RoFileSystem :> es) => ParamsDto -> Eff es Params
paramsFromDto dto = do
    projPath <- fsMkAbsolute dto.projectPath
    pure
        Params
            { projectPath = projPath
            , command = dto.command
            }

{- | One subcommand per 'Command', each with what only it is asked. A word that
names none is refused with the commands there are.
-}
pParams :: Parser ParamsDto
pParams = hsubparser (foldMap commandFor commands) <|> unknownCommand

data CommandSpec = CommandSpec
    { name :: String
    , description :: String
    , parser :: Parser Command
    }

commands :: NonEmpty CommandSpec
commands =
    CommandSpec "check" "Report the problems in the project" (pure CheckC)
        :| [ CommandSpec "fix" "Repair the auto-fixable problems" (pure FixC)
           , CommandSpec "baseline" "Accept the current problems into deslop/baseline.yaml" (pure BaselineC)
           , CommandSpec "explore" "Write an interactive module graph to deslop/diagram/index.html" (ExploreC <$> pExploration)
           ]

commandFor :: CommandSpec -> Mod CommandFields ParamsDto
commandFor spec =
    command spec.name . info (flip ParamsDto <$> pProjectPath <*> spec.parser) $ progDesc spec.description

-- | Consumes the word no subcommand matched, only to name it in the refusal. Hidden from the help.
unknownCommand :: Parser ParamsDto
unknownCommand = argument (eitherReader refuse) (metavar "COMMAND" <> internal)
  where
    refuse word = Left $ "Unknown command '" <> word <> "'. Expected: " <> expected
    expected = intercalate ", " (init names) <> ", or " <> last names
    names = (.name) <$> commands

pProjectPath :: Parser OsPath
pProjectPath =
    argument
        (eitherReader (Right . encodeOsPath . T.pack))
        ( metavar "PROJECT_DIR"
            <> help "Path to the project"
            <> value [osp|.|]
            <> showDefault
        )

pExploration :: Parser Exploration
pExploration =
    Exploration
        <$> option
            (eitherReader readGitRef)
            ( long "diff-base"
                <> metavar "REF"
                <> help "Mark the modules changed since this branch left REF (origin/REF when the remote has it)"
                <> value (GitRef "main")
                <> showDefaultWith (toString . (.text))
            )
        <*> flag OpenWhenAttended NeverOpen (long "ci" <> help "Never open the page in a browser, even on a terminal")

-- | A ref git would read as an option is no ref: refused before it reaches git.
readGitRef :: String -> Either String GitRef
readGitRef = \case
    ref@('-' : _) -> Left $ "A ref cannot start with '-': " <> ref
    ref -> Right . GitRef $ toText ref

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Deslop Version " <> showVersion version)
        ( long "version"
            <> short 'v'
            <> help "Show version"
        )

paramsParser :: ParserInfo ParamsDto
paramsParser =
    info
        (helper <*> versionOption <*> pParams)
        ( fullDesc
            <> header "Deslop - A Haskell-powered code cleaner ✨"
            <> progDesc "Removes slop from your project's architecture."
        )

parserPrefs :: ParserPrefs
parserPrefs =
    prefs $
        showHelpOnError
            <> showHelpOnEmpty
            <> helpShowGlobals
