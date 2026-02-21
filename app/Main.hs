module Main (main) where

import Data.Version (showVersion)
import Deslop (runDeslop)
import Options.Applicative
import Paths_deslop (version)
import Types

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

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Deslop Version " <> showVersion version)
        ( long "version"
            <> short 'v'
            <> help "Show version"
        )

optsInfo :: ParserInfo Params
optsInfo =
    info
        (helper <*> versionOption <*> pParams)
        ( fullDesc
            <> header "Deslop - A Haskell-powered code cleaner ✨"
            <> progDesc "Removes slop from TypeScript projects."
        )

parserPrefs :: ParserPrefs
parserPrefs =
    prefs $
        showHelpOnError -- Show full help text on any error
            <> showHelpOnEmpty -- Show full help if no args are provided
            <> helpShowGlobals -- Show global options in help


main :: IO ()
main = do
    params <- customExecParser parserPrefs optsInfo
    runDeslop params
