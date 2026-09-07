{- | Generating TypeScript source by generating the CST it must parse back to.

The point of building source this way round is that the generator /knows/ which
statements are dependencies and which only look like one. That is what lets a
property say "exactly these edges and no others", which is the claim that
matters: @deslop fix@ rewrites what the parser classifies, so a string literal
mistaken for a specifier is a rewritten string literal in somebody's source.

Every statement is emitted terminated and followed by its own newline node, so
the nodes a parse returns line up with the nodes that were planted rather than
differing only in where the whitespace landed.
-}
module Generators.TypeScript.CST (
    genTsProgram,
    genNonEdgeProgram,
    genSpecifier,
    genRelativeSpecifier,
    genAliasedSpecifier,
    Edge (..),
    edgesOf,
) where

import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import TypeScript.CST (TsNode (..))

{- | What a program is claimed to depend on: the kind of statement and the
specifier it names, in source order.
-}
data Edge = ImportOf Text | ReExportOf Text
    deriving stock (Show, Eq)

edgesOf :: [TsNode] -> [Edge]
edgesOf = mapMaybe edge
  where
    edge (Import _ t _) = Just (ImportOf t)
    edge (ReExport _ t _) = Just (ReExportOf t)
    edge (Source _) = Nothing

-- | A whole file: statements that are edges mixed with statements that are not.
genTsProgram :: Gen [TsNode]
genTsProgram = terminated <$> Gen.list (Range.linear 0 15) genNode

-- | A file with no dependencies in it at all, however much of it looks like one.
genNonEdgeProgram :: Gen [TsNode]
genNonEdgeProgram = terminated . map Source <$> Gen.list (Range.linear 0 15) genNonEdgeSource

-- | Each statement on its own line, which is how anybody actually writes them.
terminated :: [TsNode] -> [TsNode]
terminated = concatMap (\n -> [n, Source "\n"])

genNode :: Gen TsNode
genNode =
    Gen.frequency
        [ (3, genImport)
        , (3, genReExport)
        , (5, Source <$> genNonEdgeSource)
        ]

genImport :: Gen TsNode
genImport = do
    clause <- Gen.element importClauses
    quote <- genQuote
    specifier <- genSpecifier
    let from = if clause == "" then "" else clause <> " from "
    pure
        Import
            { prefix = "import " <> from <> quote
            , target = specifier
            , suffix = quote <> ";"
            }
  where
    importClauses =
        [ "{ a }"
        , "{ a as b }"
        , "{ type T, a }"
        , "type { T }"
        , "* as ns"
        , "x"
        , "x, { y }"
        , "" -- a bare side-effect import: import "./polyfill";
        ]

genReExport :: Gen TsNode
genReExport = do
    clause <- Gen.element exportClauses
    quote <- genQuote
    specifier <- genSpecifier
    pure
        ReExport
            { prefix = "export " <> clause <> " from " <> quote
            , target = specifier
            , suffix = quote <> ";"
            }
  where
    exportClauses =
        [ "*"
        , "* as ns"
        , "{ x }"
        , "{ x as y }"
        , "{ default as X }"
        , "{}"
        , "type { T }"
        , "{ type T, x }"
        ]

{- | Statements that name no module, including every shape that has ever looked
like it did: a string that could pass for a specifier, a named export with no
@from@, a word beginning with @import@, and a dependency written inside a
comment.

A /whole/ import or re-export statement inside a string literal is deliberately
absent: the lexer does classify one of those as an edge, and it is a known
limitation rather than something this generator should assert.
-}
genNonEdgeSource :: Gen Text
genNonEdgeSource =
    Gen.element
        [ "export const x = 5;"
        , "export const cfg = \"./config\";"
        , "export const alias = \"@/lib/thing\";"
        , "export const nested = { from: \"./nope\" };"
        , "export let counter = 0;"
        , "export var legacy = null;"
        , "export default function foo() { return 1; }"
        , "export default class Bar {}"
        , "export function baz(a: number) { return a; }"
        , "export async function qux() { await go(); }"
        , "export class Component {}"
        , "export abstract class Base {}"
        , "export interface Props { a: string }"
        , "export enum Colour { Red, Green }"
        , "export type Foo = { a: string };"
        , "export type { T };"
        , "export { a, b };"
        , "export { a as b };"
        , "export {};"
        , "exports.foo = 1;"
        , "module.exports = {};"
        , "const importantThing = 1;"
        , "const exported = true;"
        , "const exportedFrom = \"./x\";"
        , "let importer = null;"
        , "function importAll() { return 1; }"
        , "const a = 1;"
        , "type Handler = (e: Event) => void;"
        , "// import { x } from \"./commented\";"
        , "// export * from \"./commented\";"
        , "/* export { y } from \"./blocked\"; */"
        , "/** @see export * from \"./doc\" */"
        ]

genQuote :: Gen Text
genQuote = Gen.element ["\"", "'"]

genSpecifier :: Gen Text
genSpecifier =
    Gen.frequency
        [ (3, genRelativeSpecifier)
        , (3, genAliasedSpecifier)
        , (1, Gen.element ["react", "next/link", "@scope/pkg"])
        ]

genRelativeSpecifier :: Gen Text
genRelativeSpecifier = do
    prefix <- Gen.element ["./", "../", "../../"]
    segments <- Gen.list (Range.linear 1 3) genSegment
    pure $ prefix <> joinSegments segments

genAliasedSpecifier :: Gen Text
genAliasedSpecifier = do
    alias <- Gen.element ["@/", "@lib/", "~/"]
    segments <- Gen.list (Range.linear 1 3) genSegment
    pure $ alias <> joinSegments segments

genSegment :: Gen Text
genSegment = Gen.element ["a", "b", "lib", "util", "home", "index", "service", "deep-name"]

joinSegments :: [Text] -> Text
joinSegments [] = "x"
joinSegments segments = T.intercalate "/" segments
