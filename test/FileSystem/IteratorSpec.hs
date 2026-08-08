module FileSystem.IteratorSpec (spec) where

import Data.Text qualified as T
import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, mockDirs, mockDirsWithSymlinks, runMockRoFileSystem)
import Effectful (IOE, runEff)
import Effects.FileSystem (AbsPath (osPath), decodeOsPath)
import FileSystem.Iterator (Entry (..), walkDir)
import Test.Hspec
import TestUtils (ap)

root :: AbsPath
root = ap "/project"

everything :: Entry -> Maybe AbsPath
everything e = Just e.path

filesOnly :: Entry -> Maybe AbsPath
filesOnly e = e.path <$ guard (not e.isDir)

run :: MockRoFileSystem '[IOE] -> (Entry -> Bool) -> (Entry -> Maybe a) -> IO [a]
run mocks prune select = runEff . runMockRoFileSystem mocks $ walkDir prune select root

named :: Text -> Entry -> Bool
named n = (== n) . decodeOsPath . (.osPath) . (.path)

spec :: Spec
spec = describe "FileSystem.Iterator" $ do
    it "collects every file in the tree" $ do
        let srcDir = ap "/project/src"
        let files = [ap "/project/root.ts", ap "/project/src/a.ts", ap "/project/src/b.ts"]
        run
            (mockDirs [(root, [ap "/project/root.ts", srcDir]), (srcDir, drop 1 files)])
            (const False)
            filesOnly
            >>= (`shouldBe` files)

    it "shows directories to select as well as files" $ do
        let srcDir = ap "/project/src"
        run
            (mockDirs [(root, [srcDir]), (srcDir, [ap "/project/src/a.ts"])])
            (const False)
            everything
            >>= (`shouldBe` [srcDir, ap "/project/src/a.ts"])

    it "tags entries as directories or not" $ do
        let srcDir = ap "/project/src"
        run
            (mockDirs [(root, [srcDir, ap "/project/a.ts"]), (srcDir, [])])
            (const False)
            (Just . (.isDir))
            >>= (`shouldBe` [True, False])

    it "prunes a pruned directory's entire subtree" $ do
        let skipDir = ap "/project/skip"
        let deep = ap "/project/skip/deep"
        run
            ( mockDirs
                [ (root, [skipDir, ap "/project/keep.ts"])
                , (skipDir, [ap "/project/skip/a.ts", deep])
                , (deep, [ap "/project/skip/deep/b.ts"])
                ]
            )
            (named "/project/skip")
            filesOnly
            >>= (`shouldBe` [ap "/project/keep.ts"])

    it "drops a pruned file without affecting its siblings" $ do
        let files = [ap "/project/a.ts", ap "/project/b.ts"]
        run (mockDirs [(root, files)]) (named "/project/a.ts") filesOnly
            >>= (`shouldBe` [ap "/project/b.ts"])

    it "never returns a path beneath a pruned directory" $ do
        let skipDir = ap "/project/skip"
        result <-
            run
                ( mockDirs
                    [ (root, [skipDir])
                    , (skipDir, [ap "/project/skip/a.ts"])
                    ]
                )
                (named "/project/skip")
                everything
        filter (T.isPrefixOf "/project/skip" . decodeOsPath . (.osPath)) result `shouldBe` []

    it "returns nothing for an empty directory" $
        run (mockDirs [(root, [])]) (const False) filesOnly >>= (`shouldBe` [])

    describe "symlinks" $ do
        it "does not descend into a symlinked directory" $ do
            let link = ap "/project/link"
            run
                ( mockDirsWithSymlinks
                    [(root, [link, ap "/project/a.ts"]), (link, [ap "/project/link/hidden.ts"])]
                    [link]
                )
                (const False)
                filesOnly
                >>= (`shouldBe` [ap "/project/a.ts"])

        it "still reports the symlinked directory itself as an entry" $ do
            let link = ap "/project/link"
            run
                (mockDirsWithSymlinks [(root, [link]), (link, [])] [link])
                (const False)
                everything
                >>= (`shouldBe` [link])

        -- Every step of a symlink loop is a distinct path, so a visited-set over
        -- raw paths would never fire. Refusing to follow symlinks at all is what
        -- makes this terminate.
        it "terminates on a symlink loop" $ do
            let selfReferential =
                    defaultMockRoFileSystem
                        { mockDirectoryExists = const $ pure True
                        , mockIsSymlink = \p -> pure $ p /= root
                        , mockListDirectory = \p -> pure [ap (decodeOsPath p.osPath <> "/link")]
                        }
            run selfReferential (const False) everything
                >>= (`shouldBe` [ap "/project/link"])
