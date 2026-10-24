{-# LANGUAGE QuasiQuotes #-}

module Params (
    pParams,
    paramsParser,
    parserPrefs,
    Params (..),
    ParamsDto (..),
    Command (..),
    paramsFromDto,
)
where

import Data.Text qualified as T
import Data.Version (showVersion)
import Deslop.Command (Command (..))
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
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

readCommand :: String -> Either String Command
readCommand "check" = Right CheckC
readCommand "fix" = Right FixC
readCommand "baseline" = Right BaselineC
readCommand "diagram" = Right DiagramC
readCommand s = Left $ "Unknown command '" <> s <> "'. Expected: check, fix, baseline, or diagram"

pParams :: Parser ParamsDto
pParams =
    ParamsDto
        <$> argument
            (eitherReader readCommand)
            ( metavar "COMMAND"
                <> help "Command to run: check, fix, baseline, or diagram"
            )
        <*> argument
            (eitherReader (Right . encodeOsPath . T.pack))
            ( metavar "PROJECT_DIR"
                <> help "Path to the project"
                <> value [osp|.|]
                <> showDefault
            )

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
            <> progDesc "Removes slop from your project's architecture. Commands: check, fix, baseline, diagram."
        )

parserPrefs :: ParserPrefs
parserPrefs =
    prefs $
        showHelpOnError
            <> showHelpOnEmpty
            <> helpShowGlobals
