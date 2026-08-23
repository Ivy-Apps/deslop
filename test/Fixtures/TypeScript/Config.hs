{- | A 'TsConfig' with the two path aliases most fixtures are written against,
@\@\/@ for @src\/@ and @\@test\/@ for @test\/@, rooted at @\/home\/repo@.
-}
module Fixtures.TypeScript.Config (
    defaultTsConfig,
    emptyTsConfig,
    mkMapping,
) where

import FileSystem.Path (absPathUnsafe)
import System.OsPath (osp)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))

defaultTsConfig :: TsConfig
defaultTsConfig =
    TsConfig
        { baseUrl = absPathUnsafe [osp|/home/repo|]
        , paths =
            [ mkMapping (Wildcard "@test/" "") [Wildcard "test/" ""]
            , mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]
            ]
        }

emptyTsConfig :: TsConfig
emptyTsConfig = TsConfig {baseUrl = absPathUnsafe [osp|/home/repo|], paths = []}

mkMapping :: Pattern -> [Pattern] -> PathMapping
mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)
