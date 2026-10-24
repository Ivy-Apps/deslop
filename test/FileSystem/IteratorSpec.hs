module FileSystem.IteratorSpec (spec) where

import Data.Text qualified as T
import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, mockDirs, mockDirsWithSymlinks, runMockRoFileSystem)
import Effectful (IOE, runEff)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (osPath), ProjectRoot (..), decodeOsPath)
import Fixtures.FileSystem.Path (inProject, projectRoot)
import Test.Hspec
import TestUtils (ap)

root :: AbsPath
root = projectRoot.path

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
        let srcDir = inProject "src"
        let files = [inProject "root.ts", inProject "src/a.ts", inProject "src/b.ts"]
        run
            (mockDirs [(root, [inProject "root.ts", srcDir]), (srcDir, drop 1 files)])
            (const False)
            filesOnly
            >>= (`shouldBe` files)

    it "shows directories to select as well as files" $ do
        let srcDir = inProject "src"
        run
            (mockDirs [(root, [srcDir]), (srcDir, [inProject "src/a.ts"])])
            (const False)
            everything
            >>= (`shouldBe` [srcDir, inProject "src/a.ts"])

    it "tags entries as directories or not" $ do
        let srcDir = inProject "src"
        run
            (mockDirs [(root, [srcDir, inProject "a.ts"]), (srcDir, [])])
            (const False)
            (Just . (.isDir))
            >>= (`shouldBe` [True, False])

    it "prunes a pruned directory's entire subtree" $ do
        let skipDir = inProject "skip"
        let deep = inProject "skip/deep"
        run
            ( mockDirs
                [ (root, [skipDir, inProject "keep.ts"])
                , (skipDir, [inProject "skip/a.ts", deep])
                , (deep, [inProject "skip/deep/b.ts"])
                ]
            )
            (named "/project/skip")
            filesOnly
            >>= (`shouldBe` [inProject "keep.ts"])

    it "drops a pruned file without affecting its siblings" $ do
        let files = [inProject "a.ts", inProject "b.ts"]
        run (mockDirs [(root, files)]) (named "/project/a.ts") filesOnly
            >>= (`shouldBe` [inProject "b.ts"])

    it "never returns a path beneath a pruned directory" $ do
        let skipDir = inProject "skip"
        result <-
            run
                ( mockDirs
                    [ (root, [skipDir])
                    , (skipDir, [inProject "skip/a.ts"])
                    ]
                )
                (named "/project/skip")
                everything
        filter (T.isPrefixOf "/project/skip" . decodeOsPath . (.osPath)) result `shouldBe` []

    it "returns nothing for an empty directory" $
        run (mockDirs [(root, [])]) (const False) filesOnly >>= (`shouldBe` [])

    describe "symlinks" $ do
        it "does not descend into a symlinked directory" $ do
            let link = inProject "link"
            run
                ( mockDirsWithSymlinks
                    [(root, [link, inProject "a.ts"]), (link, [inProject "link/hidden.ts"])]
                    [link]
                )
                (const False)
                filesOnly
                >>= (`shouldBe` [inProject "a.ts"])

        it "still reports the symlinked directory itself as an entry" $ do
            let link = inProject "link"
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
                >>= (`shouldBe` [inProject "link"])
