{-# LANGUAGE QuasiQuotes #-}

module Params (
    pParams,
    paramsParser,
    parserPrefs,
    Params (..),
)
where

import Data.Text qualified as T
import Data.Version (showVersion)
import Effects.FileSystem (encodeOsPath)
import Options.Applicative
import Paths_deslop (version)
import System.OsPath (OsPath, osp)

data Params = Params
    { projectPath :: OsPath
    , checkMode :: Bool
    }
    deriving (Show, Eq)

pParams :: Parser Params
pParams =
    Params
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

paramsParser :: ParserInfo Params
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
