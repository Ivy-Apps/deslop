module Deslop.DiagramSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Deslop.Diagram (
    Arrow (..),
    Board (..),
    BoardEnd (..),
    BoardName (..),
    Diagram (..),
    Folder (..),
    Graph (..),
    Layout (..),
    Place (..),
    Spot (..),
    diagramOf,
    layoutOf,
    toD2,
 )
import Deslop.Diagram.D2 qualified as D2
import Deslop.Module (
    EdgeKind (..),
    EdgeTarget (..),
    Module (..),
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Fixtures.Deslop.Module (mkEdge, mkModuleAt)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Diagram" $ do
    describe "layoutOf" $ do
        it "collapses every module to its folder two levels below the shared prefix" $ do
            let modules =
                    [ at "src/domain/user/entities/User.ts" []
                    , at "src/domain/user/a.ts" []
                    , at "src/app/page.tsx" []
                    , at "src/main.ts" []
                    ]

            let layout = layoutOf modules

            layout.prefix `shouldBe` ["src"]
            Map.elems layout.spots
                `shouldMatchList` [ FolderFile (folder ["domain", "user"]) ("entities" :| ["User.ts"])
                                  , FolderFile (folder ["domain", "user"]) ("a.ts" :| [])
                                  , FolderFile (folder ["app"]) ("page.tsx" :| [])
                                  , RootFile "main.ts"
                                  ]

        it "strips nothing when a module sits at the project root" $ do
            let modules = [at "next.config.ts" [], at "src/app/page.tsx" []]

            (layoutOf modules).prefix `shouldBe` []

    describe "diagramOf" $ do
        it "counts dependencies per pair of folders and kind, and drops those within one folder" $ do
            let modules =
                    [ at "src/app/page.tsx" ["src/domain/user/a.ts", "src/domain/user/b.ts"]
                    , reExporting "src/app/index.ts" "src/domain/user/a.ts"
                    , at "src/domain/user/a.ts" ["src/domain/user/b.ts"]
                    , at "src/domain/user/b.ts" []
                    ]

            let d = diagramOf modules

            d.overview.arrows
                `shouldBe` Map.fromList
                    [ (Arrow (InFolder app) (InFolder user) ImportEdge, 2)
                    , (Arrow (InFolder app) (InFolder user) ReExportEdge, 1)
                    ]

        it "draws nothing for an npm package or a module nobody parsed" $ do
            let modules =
                    [ withEdge (at "src/app/page.tsx" ["src/unscanned/x.ts"]) (External $ moduleNameUnsafe "react")
                    , at "src/lib/util.ts" []
                    ]

            let d = diagramOf modules

            d.overview.arrows `shouldBe` Map.empty
            d.overview.nodes `shouldBe` Set.fromList [InFolder $ folder ["app"], InFolder $ folder ["lib"]]

        it "drills into a folder with its files apart and every other folder collapsed" $ do
            let modules =
                    [ at "src/app/page.tsx" ["src/domain/user/a.ts", "src/domain/user/sub/b.ts"]
                    , at "src/domain/user/a.ts" ["src/domain/user/sub/b.ts", "src/lib/util.ts"]
                    , at "src/domain/user/sub/b.ts" []
                    , at "src/lib/util.ts" []
                    ]

            let userBoard = find ((== user) . (.folder)) (diagramOf modules).boards

            fmap (.graph.arrows) userBoard
                `shouldBe` Just
                    ( Map.fromList
                        [ (Arrow (Local $ "a.ts" :| []) (Local $ "sub" :| ["b.ts"]) ImportEdge, 1)
                        , (Arrow (Local $ "a.ts" :| []) (Neighbour . InFolder $ folder ["lib"]) ImportEdge, 1)
                        , (Arrow (Neighbour $ InFolder app) (Local $ "a.ts" :| []) ImportEdge, 1)
                        , (Arrow (Neighbour $ InFolder app) (Local $ "sub" :| ["b.ts"]) ImportEdge, 1)
                        ]
                    )

        it "names every board for its folder in a form D2 can file it under" $ do
            let modules = [at "src/app/[locale]/page.tsx" [], at "src/Domain/User/a.ts" []]

            let names = (.name) <$> (diagramOf modules).boards

            names `shouldBe` [BoardName "domain-user", BoardName "app-locale"]

    describe "toD2" $
        it "writes the overview and a layer per folder, linked both ways" $ do
            let modules = [at "src/a/x.ts" ["src/b/y.ts"], at "src/b/y.ts" []]

            let d2 = D2.render . toD2 . diagramOf $ modules

            d2
                `shouldBe` unlines
                    [ "vars: {"
                    , "  d2-config: {"
                    , "    layout-engine: elk"
                    , "    theme-id: 0"
                    , "    dark-theme-id: 200"
                    , "  }"
                    , "}"
                    , "direction: right"
                    , "\"a\": {"
                    , "  shape: package"
                    , "  label: \"a\""
                    , "  tooltip: \"src/a\""
                    , "  link: layers.a"
                    , "}"
                    , "\"b\": {"
                    , "  shape: package"
                    , "  label: \"b\""
                    , "  tooltip: \"src/b\""
                    , "  link: layers.b"
                    , "}"
                    , "\"a\" -> \"b\": {"
                    , "  label: \"1\""
                    , "  style.stroke-width: 1"
                    , "}"
                    , "layers: {"
                    , "  \"a\": {"
                    , "    direction: right"
                    , "    \"← overview\": {"
                    , "      near: top-left"
                    , "      link: _"
                    , "    }"
                    , "    \"a\": {"
                    , "      label: \"a\""
                    , "      tooltip: \"src/a\""
                    , "    }"
                    , "    \"a\".\"x.ts\": {"
                    , "      tooltip: \"src/a/x.ts\""
                    , "    }"
                    , "    \"b\": {"
                    , "      shape: package"
                    , "      label: \"b\""
                    , "      tooltip: \"src/b\""
                    , "      link: _.layers.b"
                    , "    }"
                    , "    \"a\".\"x.ts\" -> \"b\": {"
                    , "      label: \"1\""
                    , "      style.stroke-width: 1"
                    , "    }"
                    , "  }"
                    , "  \"b\": {"
                    , "    direction: right"
                    , "    \"← overview\": {"
                    , "      near: top-left"
                    , "      link: _"
                    , "    }"
                    , "    \"b\": {"
                    , "      label: \"b\""
                    , "      tooltip: \"src/b\""
                    , "    }"
                    , "    \"b\".\"y.ts\": {"
                    , "      tooltip: \"src/b/y.ts\""
                    , "    }"
                    , "    \"a\": {"
                    , "      shape: package"
                    , "      label: \"a\""
                    , "      tooltip: \"src/a\""
                    , "      link: _.layers.a"
                    , "    }"
                    , "    \"a\" -> \"b\".\"y.ts\": {"
                    , "      label: \"1\""
                    , "      style.stroke-width: 1"
                    , "    }"
                    , "  }"
                    , "}"
                    ]
  where
    app = folder ["app"]
    user = folder ["domain", "user"]

-- | A module identified by its own path, importing the modules at these paths.
at :: Text -> [Text] -> Module
at path = mkModuleAt path path

-- | A module at this path, re-exporting the module at another.
reExporting :: Text -> Text -> Module
reExporting path dep = (at path []) {edges = [mkEdge path 1 dep (internal dep) ReExportEdge]}

withEdge :: Module -> EdgeTarget -> Module
withEdge m target = m {edges = m.edges <> [mkEdge "x" 1 "x" target ImportEdge]}

internal :: Text -> EdgeTarget
internal p = Resolved (moduleIdUnsafe p) (moduleNameUnsafe p :| [])

folder :: [Text] -> Folder
folder = maybe (error "folder: no segments") Folder . nonEmpty
