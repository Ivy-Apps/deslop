{- | A Haskell module's name as Haskell writes it - @Data.List.NonEmpty@ - and
the one place it becomes a 'Deslop.Module.ModuleName', spelled with slashes
(@docs\/adr\/0019@).
-}
module Frontend.Haskell.ModuleName (
    HsModuleName,
    hsModuleName,
    fromDotted,
    fromSegments,
    dotted,
    slashed,
    mainModule,
) where

import Data.Char (isUpper)
import Data.Text qualified as T
import Deslop.Module (ModuleName, moduleNameUnsafe)
import Frontend.Haskell.Lexer (isIdentChar)

{- | The dot-separated segments of a module name, e.g. @Data@, @Text@.
'fromDotted' and 'fromSegments' accept only what GHC would; 'hsModuleName'
accepts anything, because a file whose path is no valid module name is still
a module, named by that path.
-}
newtype HsModuleName = HsModuleName (NonEmpty Text)
    deriving stock (Show, Eq, Ord)

hsModuleName :: NonEmpty Text -> HsModuleName
hsModuleName = HsModuleName

-- | @Data.Text@, if every segment is a valid conid.
fromDotted :: Text -> Maybe HsModuleName
fromDotted = fromSegments . T.splitOn "."

-- | The segments, if each is a valid conid: what GHC would accept as a module name.
fromSegments :: [Text] -> Maybe HsModuleName
fromSegments = fmap HsModuleName . (nonEmpty <=< traverse conid)
  where
    conid segment = case T.uncons segment of
        Just (c, rest) | isUpper c && T.all isIdentChar rest -> Just segment
        _ -> Nothing

dotted :: HsModuleName -> Text
dotted (HsModuleName segments) = T.intercalate "." (toList segments)

-- | @Data/Text@: how every Module Name is spelled, in every language.
slashed :: HsModuleName -> ModuleName
slashed (HsModuleName segments) = moduleNameUnsafe . T.intercalate "/" . toList $ segments

-- | What a file without a @module@ header is: @module Main (main) where@.
mainModule :: HsModuleName
mainModule = HsModuleName ("Main" :| [])
