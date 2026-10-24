-- | Reading what a parsed 'HsSource' says about its module header.
module Fixtures.Frontend.Haskell.CST (
    headerNameOf,
    reExportsOf,
) where

import Frontend.Haskell.CST (Header (..), HsSource (..), ModuleExport, ModuleHeader (..))
import Frontend.Haskell.ModuleName (dotted)

-- | The name the header declares, or 'Nothing' for a file without one.
headerNameOf :: HsSource -> Maybe Text
headerNameOf source = case source.header of
    Declared h -> Just (dotted h.name)
    ImplicitMain -> Nothing

reExportsOf :: HsSource -> [ModuleExport]
reExportsOf source = case source.header of
    Declared h -> h.reExports
    ImplicitMain -> []
