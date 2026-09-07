{- | The property suite for lexing and parsing TypeScript.

Properties are numbered and the numbers are load-bearing - they are referenced
from @docs\/adr\/0016-a-module-is-identified-by-what-it-resolves-to.md@.

P1 says nothing is lost, which is what makes @deslop fix@ safe to run over a
file it only partly understands. P2 and P3 say nothing is invented, which is the
stronger claim: a statement that only looks like a dependency must not become
one, because what the parser classifies is what the fixer rewrites.
-}
module TypeScript.ParserPropSpec (spec) where

import Generators.TypeScript.CST (edgesOf, genNonEdgeProgram, genTsProgram)
import Hedgehog (PropertyT, annotate, failure, forAll, (===))
import Renderable (Renderable (render))
import Test.Hspec
import TestUtils (ap, prop)
import TypeScript.CST (TsNode (..), TsProgram (cst))
import TypeScript.Parser (TsFile (..), parseTs)

spec :: Spec
spec = describe "TypeScript.Parser properties" $ do
    prop "P1 parsing and rendering returns the source unchanged" $ do
        planted <- forAll genTsProgram
        let source = render planted
        parsed <- parseSource source
        render parsed === source

    prop "P2 the statements parsed are exactly the dependencies planted" $ do
        planted <- forAll genTsProgram
        parsed <- parseSource (render planted)
        edgesOf parsed === edgesOf planted

    prop "P2b a planted dependency is recovered whole, down to its quotes" $ do
        planted <- forAll genTsProgram
        parsed <- parseSource (render planted)
        filter isEdge parsed === filter isEdge planted

    prop "P3 statements that only look like dependencies yield none" $ do
        planted <- forAll genNonEdgeProgram
        parsed <- parseSource (render planted)
        edgesOf parsed === []

parseSource :: Text -> PropertyT IO [TsNode]
parseSource source =
    case parseTs TsFile {path = ap "/home/repo/src/generated.ts", content = source} of
        Left err -> annotate err >> failure
        Right prog -> pure prog.cst

isEdge :: TsNode -> Bool
isEdge Import {} = True
isEdge ReExport {} = True
isEdge Source {} = False
