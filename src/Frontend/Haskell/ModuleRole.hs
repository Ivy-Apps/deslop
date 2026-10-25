{- | Whether a Haskell module ships, tests or measures. Two signals, either
enough: the cabal component compiling it, and the frameworks it imports - a
spec under @test\/@ that no @test-suite@ lists still imports @Test.Hspec@.

A spec named as one also names what it tests: @Deslop.ModuleSpec@ and
@Deslop.ModulePropSpec@ test @Deslop.Module@, as GHC would find it from the spec.
-}
module Frontend.Haskell.ModuleRole (
    moduleRole,
    withSubject,
) where

import Data.Text qualified as T
import Deslop.Module (ModuleId, ModuleRole (..))
import Frontend.Haskell.Cabal (ComponentKind)
import Frontend.Haskell.Cabal qualified as Cabal
import Frontend.Haskell.ModuleName (HsModuleName, dotted, fromDotted)

{- | The strongest role either signal claims: a module of a @benchmark@
component that imports @Test.Tasty@ is a 'Benchmark'. Which module a test
tests takes the whole program: see 'withSubject'.
-}
moduleRole :: Maybe ComponentKind -> [HsModuleName] -> ModuleRole
moduleRole component imports = foldMap componentRole component <> foldMap importRole imports

{- | A test named after a module, given the source module that name reaches,
when it reaches one. Anything else is as it was.
-}
withSubject :: (HsModuleName -> Maybe ModuleId) -> HsModuleName -> ModuleRole -> ModuleRole
withSubject sourceNamed name = \case
    Test _ -> Test (sourceNamed =<< subjectName name)
    other -> other

{- | The name of the module a spec is named after: @hspec-discover@'s
convention, @FooSpec@ for @Foo@, and @FooPropSpec@ for the properties of @Foo@.
-}
subjectName :: HsModuleName -> Maybe HsModuleName
subjectName name = viaNonEmpty head $ mapMaybe (\suffix -> fromDotted =<< T.stripSuffix suffix (dotted name)) ["PropSpec", "Spec"]

componentRole :: ComponentKind -> ModuleRole
componentRole = \case
    Cabal.Library -> Source
    Cabal.Executable _ -> Source
    Cabal.TestSuite _ -> Test Nothing
    Cabal.Benchmark _ -> Benchmark

{- | What importing this module says. @Test.Tasty.Bench@ is under @Test.Tasty@,
so it claims both, and the stronger wins.
-}
importRole :: HsModuleName -> ModuleRole
importRole name = foldMap claims [(Benchmark, ["Criterion", "Gauge", "Test.Tasty.Bench"]), (Test Nothing, ["Test.Hspec", "Test.Tasty", "Test.HUnit"])]
  where
    claims (role, frameworks)
        | any within frameworks = role
        | otherwise = Source
    within framework = dotted name == framework || (framework <> ".") `T.isPrefixOf` dotted name
