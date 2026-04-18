{-# LANGUAGE QuasiQuotes #-}

module Params (
    pParams,
    paramsParser,
    parserPrefs,
    Params (..),
    ParamsDto (..),
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

data Params = Params
    { projectPath :: AbsPath
    , checkMode :: Bool
    }
    deriving (Show, Eq)

data ParamsDto = ParamsDto
    { projectPath :: OsPath
    , checkMode :: Bool
    }
    deriving (Show, Eq)

paramsFromDto :: (RoFileSystem :> es) => ParamsDto -> Eff es Params
paramsFromDto dto = do
    projPath <- fsMkAbsolute dto.projectPath
    pure
        Params
            { projectPath = projPath
            , checkMode = dto.checkMode
            }

pParams :: Parser ParamsDto
pParams =
    ParamsDto
        <$> argument
            (eitherReader (Right . encodeOsPath . T.pack))
            ( metavar "PROJECT_PATH"
                <> help "Path to the TypeScript project"
                <> value [osp|.|]
                <> showDefault
            )
        <*> switch
            ( long "check"
                <> short 'c'
                <> help "Check mode. Won't change files and will only report problems"
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
            <> progDesc "Removes slop from TypeScript projects."
        )

parserPrefs :: ParserPrefs
parserPrefs =
    prefs $
        showHelpOnError
            <> showHelpOnEmpty
            <> helpShowGlobals
