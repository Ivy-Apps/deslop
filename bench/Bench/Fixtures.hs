{-# LANGUAGE QuasiQuotes #-}

{- | The projects the benchmark runs against, and the cases built from them.

The list is spelled out rather than discovered by scanning @test/fixtures@ for
@tsconfig.json@ files. Discovery would silently enrol a fixture added for an
unrelated test, which changes the set the Reference was recorded against
without anyone deciding to.
-}
module Bench.Fixtures (
    Fixture (..),
    Case (..),
    fixtures,
    cases,
    fixturePath,
    commandName,
) where

import Effects.FileSystem (encodeOsPathString)
import Params (Command (..))
import System.OsPath (OsPath, osp, (</>))

{- | One TypeScript project under @test/fixtures@ that the benchmark measures.
-}
newtype Fixture = Fixture
    { name :: Text
    }
    deriving stock (Show, Eq, Ord)

-- | One measured combination: a Fixture run under one Command.
data Case = Case
    { fixture :: Fixture
    , command :: Command
    }
    deriving stock (Show, Eq)

{- | Every valid fixture project, roughly largest first, so that each group of
the report opens with the rows carrying the most signal.

Excluded on purpose: @ts-invalid-rulebook-project@, whose rulebook fails to
load, so a run of it measures error handling rather than Deslop's work;
@rulebook@, @secrets@, @static@ and @typescript@, which are not projects and
have no @tsconfig.json@; and @ts-cycles-project@, six files that finish in half
a millisecond, which is short enough that the measurement was mostly scheduling
noise and its cycle detection is covered by the larger fixtures anyway.
-}
fixtures :: [Fixture]
fixtures =
    Fixture
        <$> [ "nikolovlazar-nextjs-clean-architecture"
            , "ixartz-next-js-boilerplate"
            , "ts-project-1"
            , "ts-casing-project"
            , "ts-globplus-project"
            , "ts-gitignore-project"
            ]

{- | Every case, grouped by Command so the report reads group by group.

@baseline@ is measured in its own right rather than only inside a combined
total: it is the sole path through 'Deslop.Baseline.saveBaseline' and can
regress on its own.
-}
cases :: [Case]
cases = [Case f c | c <- [CheckC, FixC, BaselineC], f <- fixtures]

fixturePath :: Fixture -> OsPath
fixturePath f = [osp|test/fixtures|] </> encodeFixtureName f
  where
    encodeFixtureName = encodeOsPathString . toString . (.name)

commandName :: Command -> Text
commandName CheckC = "check"
commandName FixC = "fix"
commandName BaselineC = "baseline"
