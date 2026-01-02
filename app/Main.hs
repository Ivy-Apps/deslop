module Main (main) where

import Deslop (Params (..), runDeslop)
import Options.Applicative

pParams :: Parser Params
pParams =
    Params
        <$> strArgument
            ( metavar "PROJECT_PATH"
                <> help "Path to the TypeScript project"
            )
        <*> switch
            ( long "imports"
                <> short 'i'
                <> help "Optimize imports"
            )
        <*> switch
            ( long "comments"
                <> short 'c'
                <> help "Remove AI-generated slop comments"
            )

optsInfo :: ParserInfo Params
optsInfo =
    info
        (helper <*> pParams)
        ( fullDesc
            <> header "Deslop - A Haskell-powered code cleaner ✨"
            <> progDesc "Removes slop from TypeScript projects."
        )

myPrefs :: ParserPrefs
myPrefs =
    prefs $
        showHelpOnError -- Show full help text on any error
            <> showHelpOnEmpty -- Show full help if no args are provided
            <> helpShowGlobals -- Show global options in help

applyDefaults :: Params -> Params
applyDefaults p@(Params _ i c)
    | not i && not c = p {imports = True, comments = True}
    | otherwise = p

main :: IO ()
main = do
    params <- applyDefaults <$> customExecParser myPrefs optsInfo
    runDeslop params
