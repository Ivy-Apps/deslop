module Params
    ( pParams
    , paramsParser
    , parserPrefs
    , Params(..)
    )
where

import Data.Version (showVersion)
import Options.Applicative
import Paths_deslop (version)

data Params = Params
    { projectPath :: FilePath
    , modifiedOnly :: Bool
    , checkMode :: Bool
    }
    deriving (Show, Eq)

pParams :: Parser Params
pParams =
    Params
        <$> strArgument
            ( metavar "PROJECT_PATH"
                <> help "Path to the TypeScript project"
                <> value "."
                <> showDefault
            )
        <*> switch
            ( long "modified"
                <> short 'm'
                <> help "Inspect only modified files in your branch (i.e. the git diff with main)"
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

