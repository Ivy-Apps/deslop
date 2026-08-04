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
import Effectful (Eff, (:>))
import Effects.FileSystem (AbsPath, RoFileSystem, encodeOsPath, fsMkAbsolute)
import Options.Applicative
import Paths_deslop (version)
import System.OsPath (OsPath, osp)

data Command = CheckC | FixC | BaselineC
    deriving (Show, Eq)

data Params = Params
    { projectPath :: AbsPath
    , command :: Command
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
readCommand s = Left $ "Unknown command '" <> s <> "'. Expected: check, fix, or baseline"

pParams :: Parser ParamsDto
pParams =
    ParamsDto
        <$> argument
            (eitherReader readCommand)
            ( metavar "COMMAND"
                <> help "Command to run: check, fix, or baseline"
            )
        <*> argument
            (eitherReader (Right . encodeOsPath . T.pack))
            ( metavar "PROJECT_DIR"
                <> help "Path to the TypeScript project"
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
            <> progDesc "Removes slop from TypeScript projects. Commands: check, fix, baseline."
        )

parserPrefs :: ParserPrefs
parserPrefs =
    prefs $
        showHelpOnError
            <> showHelpOnEmpty
            <> helpShowGlobals
