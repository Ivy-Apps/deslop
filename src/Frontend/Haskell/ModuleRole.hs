{- | Whether a Haskell module ships, tests or measures. Two signals, either
enough: the cabal component compiling it, and the frameworks it imports - a
spec under @test\/@ that no @test-suite@ lists still imports @Test.Hspec@.
-}
module Frontend.Haskell.ModuleRole (moduleRole) where

import Data.Text qualified as T
import Deslop.Module (ModuleRole (..))
import Frontend.Haskell.Cabal (ComponentKind)
import Frontend.Haskell.Cabal qualified as Cabal
import Frontend.Haskell.ModuleName (HsModuleName, dotted)

{- | The strongest role either signal claims: a module of a @benchmark@
component that imports @Test.Tasty@ is a 'Benchmark'.
-}
moduleRole :: Maybe ComponentKind -> [HsModuleName] -> ModuleRole
moduleRole component imports = foldMap componentRole component <> foldMap importRole imports

componentRole :: ComponentKind -> ModuleRole
componentRole = \case
    Cabal.Library -> Source
    Cabal.Executable _ -> Source
    Cabal.TestSuite _ -> Test
    Cabal.Benchmark _ -> Benchmark

{- | What importing this module says. @Test.Tasty.Bench@ is under @Test.Tasty@,
so it claims both, and the stronger wins.
-}
importRole :: HsModuleName -> ModuleRole
importRole name = foldMap claims [(Benchmark, ["Criterion", "Gauge", "Test.Tasty.Bench"]), (Test, ["Test.Hspec", "Test.Tasty", "Test.HUnit"])]
  where
    claims (role, frameworks)
        | any within frameworks = role
        | otherwise = Source
    within framework = dotted name == framework || (framework <> ".") `T.isPrefixOf` dotted name
