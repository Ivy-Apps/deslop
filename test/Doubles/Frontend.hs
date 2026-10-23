{-# LANGUAGE QuasiQuotes #-}

{- | A language small enough to read at a glance, for testing what every
language shares: the driver, the registry.

A Lines project has a @lines.project@ at its root and its sources are @.lines@
files. Each line is a statement. @TODO@ breaks a rule and is repaired to
@DONE@; @FIXME@ breaks another that cannot be repaired; a line starting with @!@
does not parse. Every file lowers to a 'Module' with no dependencies, named by
its path.
-}
module Doubles.Frontend (
    LinesFrontend (..),
    LinesFile (..),
    LinesParseError (..),
    todoRule,
    fixmeRule,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (fixable, unfixable)
import Deslop.Module (Location (..), Module (..), moduleIdUnsafe, moduleNameUnsafe)
import Deslop.Problem (LintRuleId (..), Problem (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful.Reader.Static (ask)
import Effects.FileSystem (fsFileExists)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, portablePath, relativePathTo, withAbsBaseUnsafe)
import Git.Ignore (isIgnored)
import Renderable (Renderable)
import Renderable qualified
import System.OsPath (osp, takeExtension, takeFileName)

data LinesFrontend = LinesFrontend
    deriving stock (Show, Eq)

data LinesFile = LinesFile
    { path :: AbsPath
    , statements :: [Text]
    }
    deriving stock (Show, Eq)

-- | A statement starts with @!@, or the bytes are not UTF-8.
data LinesParseError = LinesParseError
    deriving stock (Show, Eq)

instance Renderable LinesParseError where
    render LinesParseError = "cannot parse"

todoRule :: LintRuleId
todoRule = LintRuleId "no-todo"

fixmeRule :: LintRuleId
fixmeRule = LintRuleId "no-fixme"

instance CompilerFrontend LinesFrontend where
    type Config LinesFrontend = ()
    type Unit LinesFrontend = LinesFile
    type ParseError LinesFrontend = LinesParseError

    detect projectRoot =
        bool Nothing (Just LinesFrontend)
            <$> fsFileExists (withAbsBaseUnsafe projectRoot.path [osp|lines.project|])
    label _ = "Lines"
    lookingFor _ = "Lines (lines.project)"
    codeFence _ = CodeFence "lines"
    builtInRules _ = [todoRule, fixmeRule]
    ecosystemIgnored _ = (== [osp|generated|]) . takeFileName . (.osPath) . (.path)
    loadConfig _ _ = pure ()
    discover _ _ projectRoot gitIgnore = walkDir (isIgnored gitIgnore) source projectRoot.path
      where
        source entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.lines|])
    parse _ _ path bytes = case T.splitOn "\n" <$> decodeUtf8' bytes of
        Right statements | not (any ("!" `T.isPrefixOf`) statements) -> Right LinesFile {path, statements}
        _ -> Left LinesParseError
    render _ = TE.encodeUtf8 . T.intercalate "\n" . (.statements)
    lint _ _ file = do
        projectRoot <- ask @ProjectRoot
        let located = relativePathTo projectRoot file.path
        pure . catMaybes . zipWith (findingAt located) [1 ..] $ file.statements
      where
        findingAt located line statement = case statement of
            "TODO" -> Just . fixable (problemAt todoRule located line statement) . Endo $ repair line
            "FIXME" -> Just . unfixable $ problemAt fixmeRule located line statement
            _ -> Nothing
        repair line f = f {statements = zipWith (\i s -> bool s "DONE" (i == line)) [1 ..] f.statements}
        problemAt rule located line statement autoFixable =
            LintProblem
                { lintRule = rule
                , location = Location {file = located, line, code = statement}
                , description = "Unfinished work."
                , fix = "Finish it."
                , autoFixable
                }
    lower _ _ files = do
        projectRoot <- ask @ProjectRoot
        pure $ moduleOf projectRoot <$> files
      where
        moduleOf projectRoot file =
            let located = relativePathTo projectRoot file.path
             in Module
                    { id = moduleIdUnsafe (decodeOsPath file.path.osPath)
                    , names = moduleNameUnsafe (portablePath located) :| []
                    , path = located
                    , edges = []
                    }
