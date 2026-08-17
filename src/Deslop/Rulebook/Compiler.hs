{- | Turning a raw 'RulebookDto' into a valid 'Rulebook', or into every reason
it cannot be one.

Errors /accumulate/: a rulebook with five broken patterns reports five, because
fixing a rulebook one error per run is a poor way to spend an afternoon. The
one place accumulation stops is inside a rule, and for a reason: a clause
compiles against the variables its target binds, so a rule whose target failed
has no scope to check its clauses against. Such a rule reports its target and
its excludes - which bind nothing and so need no scope - and stays quiet about
its clauses rather than blaming each of them for a variable the target never
got to define.

Polarity is fixed here, at the six sites below, and is never a caller's choice.
-}
module Deslop.Rulebook.Compiler (
    compileRulebook,
    CompileError (..),
    Field (..),
    renderCompileError,
) where

import Data.Text qualified as T
import Deslop.GlobPlus (CompiledTargetPattern (..), Polarity (..))
import Deslop.GlobPlus.Compiler (GlobPlusError, compileClausePattern, compileExcludePattern, compileTargetPattern, renderGlobPlusError)
import Deslop.Rulebook
import Deslop.Rulebook.Dto
import Utils (Validation (..), invalid, validate)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Which field of a rule a pattern came from.
data Field
    = TargetField
    | ExcludeField
    | ForbidsField
    | AllowsField
    | UsesField
    | ExistsField
    deriving stock (Show, Eq)

fieldName :: Field -> Text
fieldName TargetField = "target"
fieldName ExcludeField = "exclude"
fieldName ForbidsField = "forbids.import"
fieldName AllowsField = "allows.import"
fieldName UsesField = "uses.import"
fieldName ExistsField = "exists.module"

-- | One reason a raw rulebook cannot become a 'Rulebook'.
data CompileError = CompileError
    { rule :: RuleId
    , field :: Field
    , glob :: Text
    , cause :: GlobPlusError
    }
    deriving stock (Show, Eq)

{- | One error, indented under the rule and field it came from. The file
heading is added by whoever knows the file name - see
"Deslop.Rulebook.Loader".
-}
renderCompileError :: CompileError -> Text
renderCompileError err =
    "  rule '"
        <> ruleText
        <> "'\n    "
        <> fieldName err.field
        <> ": \""
        <> err.glob
        <> "\"\n"
        <> indent (renderGlobPlusError err.cause)
  where
    RuleId ruleText = err.rule
    indent = T.intercalate "\n" . fmap ("      " <>) . T.lines

--------------------------------------------------------------------------------
-- Compiling
--------------------------------------------------------------------------------

compileRulebook :: RulebookDto -> Either (NonEmpty CompileError) Rulebook
compileRulebook dto = (.either') $ build <$> traverse compileRule dto.rules
  where
    build rules =
        Rulebook
            { id = RulebookId dto.id
            , name = dto.name
            , description = dto.description
            , rules = rules
            }

{- | The target compiles first, because the variables it binds are the scope
its clauses compile in. That dependency is the one place a rule cannot
accumulate, so it is written as a case rather than hidden behind an operator.
-}
compileRule :: RuleDto -> Validation (NonEmpty CompileError) Rule
compileRule dto = case compileTargetPattern glob of
    Left cause -> invalid (one (CompileError dto.id TargetField glob cause)) <* excludes
    Right target -> assemble target <$> excludes <*> clauses target.boundVars
  where
    GlobDto glob = dto.target

    excludes = optional' (globs ExcludeField compileExcludePattern) dto.exclude

    clauses bound =
        (,,,)
            <$> optional' (forbidsClause bound) dto.forbids
            <*> optional' (allowsClause bound) dto.allows
            <*> optional' (usesClause bound) dto.uses
            <*> optional' (existsClause bound) dto.exists

    assemble target exclude (forbids, allows, uses, exists) =
        Rule
            { id = dto.id
            , description = dto.description
            , target = target
            , exclude = exclude
            , forbids = forbids
            , allows = allows
            , uses = uses
            , exists = exists
            , example = dto.example
            , fix = dto.fix
            }

    globs field compile (GlobDto text) = compiled dto.id field compile text

    forbidsClause bound (ForbidsImportDto glob' transitive) =
        (\target -> ForbidsImport target (fromMaybe False transitive))
            <$> clause bound Widen ForbidsField glob'
    allowsClause bound (AllowsImportDto glob') =
        AllowsImport <$> clause bound Narrow AllowsField glob'
    usesClause bound (UsesImportDto glob' transitive) =
        (\target -> UsesImport target (fromMaybe False transitive))
            <$> clause bound Narrow UsesField glob'
    existsClause bound (ExistsModuleDto glob') =
        ExistsModule <$> clause bound Narrow ExistsField glob'

    clause bound polarity field (GlobDto text) =
        compiled dto.id field (compileClausePattern polarity bound) text

-- | Compiles one pattern, labelling any failure with the rule and field.
compiled ::
    RuleId ->
    Field ->
    (Text -> Either GlobPlusError a) ->
    Text ->
    Validation (NonEmpty CompileError) a
compiled ruleId field compile text =
    validate . first (one . CompileError ruleId field text) $ compile text

-- | Compiles an optional list, keeping it optional and non-empty.
optional' ::
    (dto -> Validation (NonEmpty CompileError) a) ->
    Maybe [dto] ->
    Validation (NonEmpty CompileError) (Maybe (NonEmpty a))
optional' _ Nothing = pure Nothing
optional' compile (Just dtos) = nonEmpty <$> traverse compile dtos
