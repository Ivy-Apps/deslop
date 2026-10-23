{-# LANGUAGE QuasiQuotes #-}

{- | A language small enough to read at a glance, for testing what every
language shares: the driver, the registry.

A Lines project has a @lines.project@ at its root and its sources are @.lines@
files. Each line is a statement. @TODO@ breaks a rule and is repaired to
@DONE@; @FIXME@ breaks another that cannot be repaired. A file whose bytes are
not UTF-8 is kept as those bytes, unread, as every frontend's 'parse' is total.
Every file lowers to a 'Module' with no dependencies, named by its path.
-}
module Doubles.Frontend (
    LinesFrontend (..),
    LinesUnit (..),
    LinesFile (..),
    LinesLowerError (..),
    LinesConfigError,
    todoRule,
    fixmeRule,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (fixable, unfixable, within)
import Deslop.Module (Location (..), Module (..), moduleIdUnsafe, moduleNameUnsafe)
import Deslop.Problem (LintRuleId (..), Problem (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful.Reader.Static (ask)
import Effects.FileSystem (fsFileExists)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (..), ProjectRelativePath, ProjectRoot (..), decodeOsPath, portablePath, relativePathTo, withAbsBaseUnsafe)
import Git.Ignore (isIgnored)
import Renderable (Renderable)
import Renderable qualified
import System.OsPath (osp, takeExtension, takeFileName)

data LinesFrontend = LinesFrontend
    deriving stock (Show, Eq)

-- | A file as read: its statements, or the bytes themselves when they are not UTF-8.
data LinesUnit
    = Readable LinesFile
    | NotUtf8 AbsPath ByteString
    deriving stock (Show, Eq)

data LinesFile = LinesFile
    { path :: AbsPath
    , statements :: [Text]
    }
    deriving stock (Show, Eq)

newtype LinesLowerError = StatementsNotRead ProjectRelativePath
    deriving stock (Show, Eq)

instance Renderable LinesLowerError where
    render (StatementsNotRead path) = portablePath path <> " is not UTF-8"

-- | A Lines project has nothing to configure, so its configuration never fails.
data LinesConfigError
    deriving stock (Show, Eq)

instance Renderable LinesConfigError where
    render e = case e of {}

todoRule :: LintRuleId
todoRule = LintRuleId "no-todo"

fixmeRule :: LintRuleId
fixmeRule = LintRuleId "no-fixme"

instance CompilerFrontend LinesFrontend where
    type Config LinesFrontend = ()
    type ConfigError LinesFrontend = LinesConfigError
    type Unit LinesFrontend = LinesUnit
    type LowerError LinesFrontend = LinesLowerError

    detect projectRoot =
        bool Nothing (Just LinesFrontend)
            <$> fsFileExists (withAbsBaseUnsafe projectRoot.path [osp|lines.project|])
    label _ = "Lines"
    lookingFor _ = "Lines (lines.project)"
    codeFence _ = CodeFence "lines"
    builtInRules _ = [todoRule, fixmeRule]
    ecosystemIgnored _ _ = (== [osp|generated|]) . takeFileName . (.osPath) . (.path)
    loadConfig _ _ = pure $ Right ((), [])
    discover _ _ projectRoot gitIgnore = walkDir (isIgnored gitIgnore) source projectRoot.path
      where
        source entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.lines|])
    parse _ _ path bytes = case decodeUtf8' bytes of
        Right content -> Readable LinesFile {path, statements = T.splitOn "\n" content}
        Left _ -> NotUtf8 path bytes
    render _ = \case
        Readable file -> TE.encodeUtf8 . T.intercalate "\n" $ file.statements
        NotUtf8 _ bytes -> bytes
    lint _ _ = \case
        Readable file -> do
            projectRoot <- ask @ProjectRoot
            let located = relativePathTo projectRoot file.path
            pure . fmap (within onReadable) . catMaybes . zipWith (findingAt located) [1 ..] $ file.statements
        NotUtf8 {} -> pure []
      where
        onReadable (Endo edit) = Endo $ \case
            Readable file -> Readable (edit file)
            unread@NotUtf8 {} -> unread
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
    lower _ _ units = do
        projectRoot <- ask @ProjectRoot
        pure
            ( moduleOf projectRoot . pathOf <$> units
            , [StatementsNotRead (relativePathTo projectRoot path) | NotUtf8 path _ <- units]
            )
      where
        pathOf = \case
            Readable file -> file.path
            NotUtf8 path _ -> path
        moduleOf projectRoot path =
            let located = relativePathTo projectRoot path
             in Module
                    { id = moduleIdUnsafe (decodeOsPath path.osPath)
                    , names = moduleNameUnsafe (portablePath located) :| []
                    , path = located
                    , edges = []
                    }
