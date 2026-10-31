module Git.SourceLinkSpec (spec) where

import FileSystem.Path (ProjectRoot (..))
import Git.SourceLink (Branch (..), Checkout (..), DiffAnchors (..), Forge (..), OriginUrl (..), ProjectName (..), PullRequest (..), Revision (..), SourceBase (..), SourceLinks (..), projectNameOf, pullRequestFromGh, sourceLinksOf)
import Test.Hspec
import TestUtils (ap)

spec :: Spec
spec = describe "Git.SourceLink" $ do
    describe "sourceLinksOf" $ do
        it "links a GitHub https remote to the branch's blob view" $
            linkOf "https://github.com/Ivy-Apps/deslop.git" (onBranch "main") ""
                `shouldBe` Just "https://github.com/Ivy-Apps/deslop/blob/main/"

        it "reads the same repository from an scp-like SSH remote" $
            linkOf "git@github.com:Ivy-Apps/deslop.git" (onBranch "main") ""
                `shouldBe` Just "https://github.com/Ivy-Apps/deslop/blob/main/"

        it "reads the same repository from an ssh:// remote with a port" $
            linkOf "ssh://git@github.com:22/Ivy-Apps/deslop" (onBranch "main") ""
                `shouldBe` Just "https://github.com/Ivy-Apps/deslop/blob/main/"

        it "drops credentials and a trailing slash from an https remote" $
            linkOf "https://user:token@github.com/Ivy-Apps/deslop/" (onBranch "main") ""
                `shouldBe` Just "https://github.com/Ivy-Apps/deslop/blob/main/"

        it "links a GitLab remote, groups included, to its -/blob view" $
            linkOf "git@gitlab.com:group/sub/repo.git" (onBranch "main") ""
                `shouldBe` Just "https://gitlab.com/group/sub/repo/-/blob/main/"

        it "recognises a forge's host whatever its case" $
            linkOf "https://GitHub.com/o/r" (onBranch "main") ""
                `shouldBe` Just "https://GitHub.com/o/r/blob/main/"

        it "has no link on a host that only spells a forge's name" $ do
            linkOf "https://notgithub.com/o/r" (onBranch "main") "" `shouldBe` Nothing
            linkOf "https://github.com.evil.example/o/r" (onBranch "main") "" `shouldBe` Nothing
            linkOf "https://gitlab.example.com/team/repo" (onBranch "main") "" `shouldBe` Nothing

        it "has no link on a host it does not know" $
            linkOf "https://bitbucket.org/o/r.git" (onBranch "main") "" `shouldBe` Nothing

        it "has no link for a remote that is a local path" $
            linkOf "/srv/git/repo.git" (onBranch "main") "" `shouldBe` Nothing

        it "keeps the slashes of a branch and of the project's folder, encoding the rest" $
            linkOf "https://github.com/o/r" (onBranch "feature/d2 diagram") "fixtures/my app/"
                `shouldBe` Just "https://github.com/o/r/blob/feature/d2%20diagram/fixtures/my%20app/"

        it "links a detached checkout to its commit" $
            linkOf "https://github.com/o/r" (Detached "0123abcd") "sub/"
                `shouldBe` Just "https://github.com/o/r/blob/0123abcd/sub/"

        it "anchors a GitHub file's diff at its path in the repository, keeping the project's folder raw" $
            forgeOf "https://github.com/Ivy-Apps/deslop.git" (onBranch "feature/d2-diagram") "fixtures/my app/"
                `shouldBe` Just (GitHub DiffAnchors {repoPrefix = "fixtures/my app/"})

        it "has no diff anchors on GitLab" $
            forgeOf "git@gitlab.com:group/repo.git" (onBranch "feature/x") "" `shouldBe` Just GitLab

    describe "projectNameOf" $ do
        it "names a project after its repository, from each kind of remote" $ do
            nameFrom "git@github.com:Ivy-Apps/deslop.git" `shouldBe` "deslop"
            nameFrom "https://github.com/a/b" `shouldBe` "b"
            nameFrom "ssh://git@host:22/group/sub/repo.git" `shouldBe` "repo"
            nameFrom "git://host/repo" `shouldBe` "repo"

        it "ignores a trailing slash, before or after .git" $ do
            nameFrom "https://github.com/a/b/" `shouldBe` "b"
            nameFrom "https://github.com/a/b.git/" `shouldBe` "b"

        it "ignores the newline git prints after a remote" $
            nameFrom "https://github.com/a/b.git\n" `shouldBe` "b"

        it "names a project on a host no forge is known for" $
            nameFrom "https://git.example.org/team/tool.git" `shouldBe` "tool"

        it "keeps a .git that is not the suffix" $
            nameFrom "https://host/a/b.github.io" `shouldBe` "b.github.io"

        it "falls back to the project's folder without an origin" $
            nameOf Nothing `shouldBe` "app"

        it "falls back to the project's folder when the remote names no repository" $ do
            nameFrom "https://github.com/" `shouldBe` "app"
            nameFrom "https://github.com/.git" `shouldBe` "app"
            nameFrom "/srv/git/repo.git" `shouldBe` "app"
            nameFrom "" `shouldBe` "app"

    describe "pullRequestFromGh" $ do
        it "reads the url of what gh pr view prints" $
            pullRequestFromGh "{\"number\":244,\"url\":\"https://github.com/Ivy-Apps/deslop/pull/244\"}\n"
                `shouldBe` Just (PullRequest "https://github.com/Ivy-Apps/deslop/pull/244")

        it "has none without a url" $
            pullRequestFromGh "{\"number\":244}" `shouldBe` Nothing

        it "has none in what is not JSON" $
            pullRequestFromGh "no pull requests found for branch \"main\"" `shouldBe` Nothing
  where
    linkOf origin revision prefix = (.source.url) <$> linksOf origin revision prefix
    forgeOf origin revision prefix = (.forge) <$> linksOf origin revision prefix
    linksOf origin revision prefix = sourceLinksOf Checkout {originUrl = OriginUrl origin, revision, prefix}
    onBranch = OnBranch . Branch
    nameFrom = nameOf . Just . OriginUrl
    nameOf = (.text) . projectNameOf (ProjectRoot $ ap "/home/me/app")
