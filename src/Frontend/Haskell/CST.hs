{- | A Haskell file as Deslop reads it: an island grammar over its tokens.

What is read is the module header, its @module X@ exports, and the import
section; everything from the first top-level declaration that is not an
@import@ onwards is the 'body', kept as tokens and never interpreted. Every
token of the file sits in exactly one place here, so rendering gives the file
back byte for byte.
-}
module Frontend.Haskell.CST (
    HsProgram (..),
    HsSource (..),
    Header (..),
    ModuleHeader (..),
    ModuleExport (..),
    Import (..),
    ImportDecl (..),
    declaredName,
) where

import FileSystem.Path (AbsPath)
import Frontend.Haskell.ModuleName (HsModuleName, mainModule)
import Frontend.Haskell.Tokens (HsToken)
import Renderable (Renderable (..))

data HsProgram = HsProgram
    { path :: AbsPath
    , source :: HsSource
    }
    deriving stock (Show, Eq)

data HsSource = HsSource
    { leading :: [HsToken]
    -- ^ Whitespace, comments and pragmas before the first token that counts:
    -- where @LANGUAGE@ and @OPTIONS_GHC@ pragmas live.
    , header :: Header
    , opening :: [HsToken]
    -- ^ Between the header and the first declaration: trivia, and the @{@ of
    -- an explicitly braced module.
    , imports :: [Import]
    , body :: [HsToken]
    -- ^ From the first top-level declaration that is not an import to the end.
    }
    deriving stock (Show, Eq)

data Header
    = -- | No @module@ line, which Haskell reads as @module Main (main) where@.
      ImplicitMain
    | Declared ModuleHeader
    deriving stock (Show, Eq)

data ModuleHeader = ModuleHeader
    { name :: HsModuleName
    , line :: Int
    , reExports :: [ModuleExport]
    -- ^ Only the @module X@ items of the export list: the others name no module.
    , tokens :: [HsToken]
    -- ^ From @module@ to @where@.
    }
    deriving stock (Show, Eq)

-- | @module X@ in an export list: whatever the imports named @X@ bring into scope.
data ModuleExport = ModuleExport
    { name :: HsModuleName
    , line :: Int
    , code :: Text
    }
    deriving stock (Show, Eq)

data Import = Import
    { decl :: ImportDecl
    , line :: Int
    , own :: [HsToken]
    -- ^ The declaration itself, from @import@ to its last token.
    , trailing :: [HsToken]
    -- ^ The trivia and the @;@ between it and the next declaration.
    }
    deriving stock (Show, Eq)

{- | What an import says about which module it reaches. Whether it is
qualified, its @SOURCE@ pragma and its import list do not change that, so they
stay in the tokens.
-}
data ImportDecl = ImportDecl
    { package :: Maybe Text
    -- ^ @import "text" Data.Text@, under @PackageImports@.
    , moduleName :: HsModuleName
    , alias :: Maybe HsModuleName
    -- ^ @as T@.
    }
    deriving stock (Show, Eq)

-- | The name the module gives itself.
declaredName :: HsSource -> HsModuleName
declaredName source = case source.header of
    ImplicitMain -> mainModule
    Declared h -> h.name

instance Renderable HsSource where
    render source =
        render source.leading
            <> render source.header
            <> render source.opening
            <> foldMap render source.imports
            <> render source.body

instance Renderable Header where
    render ImplicitMain = ""
    render (Declared h) = render h.tokens

instance Renderable Import where
    render i = render i.own <> render i.trailing
