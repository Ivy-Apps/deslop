{- | A 'TsConfig' with the two path aliases most fixtures are written against,
@\@\/@ for @src\/@ and @\@test\/@ for @test\/@, rooted at @\/home\/repo@.
-}
module Fixtures.Frontend.TypeScript.Config (
    defaultTsConfig,
    emptyTsConfig,
    mkMapping,
) where

import FileSystem.Path (absPathUnsafe)
import Frontend.TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import System.OsPath (osp)

defaultTsConfig :: TsConfig
defaultTsConfig =
    TsConfig
        { pathsBase = absPathUnsafe [osp|/home/repo|]
        , paths =
            [ mkMapping (Wildcard "@test/" "") [Wildcard "test/" ""]
            , mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]
            ]
        }

emptyTsConfig :: TsConfig
emptyTsConfig = TsConfig {pathsBase = absPathUnsafe [osp|/home/repo|], paths = []}

mkMapping :: Pattern -> [Pattern] -> PathMapping
mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)
