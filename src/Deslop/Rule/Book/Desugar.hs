{- | The core rules language, and the pass that reaches it.

A rulebook is written in a large language and compiled from a small one. The
large language is "Deslop.Rule.Book.Dto" - everything an author may write. The
small one is here: the same thing with every convenience spelled out longhand,
and the only shape "Deslop.Rule.Book.Compiler" ever sees.

The point of the split is that features can be added to the surface without the
compiler, the matcher or the enforcer learning anything new. A feature belongs
here only if all four of these hold:

1. /Structural/ - it rewrites which clauses a rule has, never the inside of a
   glob. Globs are text at this stage; parsing them is the compiler's job.
2. /Expressible/ - what it produces uses only what the core already has.
3. /Bounded/ - the size of the expansion follows from what was written, not
   from a constant chosen to be big enough.
4. /Compositional/ - it survives the combinator its clauses sit under. Repeated
   @forbids@ and @allows@ are a disjunction, but repeated @uses@ and @exists@
   are a conjunction, so a rewrite that turns one clause into many means
   something quite different in each pair.

Anything failing one of them is a change to the core, not sugar over it - which
is why @..*@ is a Glob+ primitive and not a rewrite into repeated @..@. See
@docs/adr/0014@ and @docs/adr/0015@.

The whole pass is therefore /total/: no errors, no scope, no IO. That is the
invariant, not an accident of how little sugar there is today. The day a sugar
feature needs to fail is the day it stopped being sugar, and it should be
implemented somewhere that can say so properly.
-}
module Deslop.Rule.Book.Desugar (
    DesugaredRuleDto (..),
    desugarRulebook,
    desugarRule,
) where

import Deslop.Rule.Book (RuleId)
import Deslop.Rule.Book.Dto (AllowsDto, ExistsDto, ForbidsDto (..), GlobDto (..), RuleDto (..), RulebookDto, UsesDto)

{- | A rule with no sugar left in it. Identical to a 'RuleDto' but for the
absence of @allows-only@, which is gone rather than empty: the compiler cannot
be handed one, so it cannot forget to handle one.
-}
data DesugaredRuleDto = DesugaredRuleDto
    { id :: !RuleId
    , description :: !Text
    , target :: !GlobDto
    , exclude :: Maybe [GlobDto]
    , forbids :: Maybe [ForbidsDto]
    , allows :: Maybe [AllowsDto]
    , uses :: Maybe [UsesDto]
    , exists :: Maybe [ExistsDto]
    , example :: Maybe Text
    , fix :: !Text
    }
    deriving stock (Show, Eq)

desugarRulebook :: RulebookDto RuleDto -> RulebookDto DesugaredRuleDto
desugarRulebook = fmap desugarRule

{- | @allows-only@ is the one piece of sugar there is: it says \"of everything,
these and nothing else\", which is what a @forbids@ of @**@ and an @allows@ of
the listed imports already say together.

The two lists are /appended/ to rather than replaced, so a rule may say both.
That combination is meaningful: the generated @forbids@ covers direct imports
only, so a hand-written transitive one still adds something, and every @allows@
of the rule carves out of every @forbids@ of it either way.
-}
desugarRule :: RuleDto -> DesugaredRuleDto
desugarRule dto =
    DesugaredRuleDto
        { id = dto.id
        , description = dto.description
        , target = dto.target
        , exclude = dto.exclude
        , forbids = forbids
        , allows = allows
        , uses = dto.uses
        , exists = dto.exists
        , example = dto.example
        , fix = dto.fix
        }
  where
    (forbids, allows) = case dto.allowsOnly of
        Nothing -> (dto.forbids, dto.allows)
        Just only ->
            ( Just (fromMaybe [] dto.forbids <> [forbidsEverything])
            , Just (fromMaybe [] dto.allows <> only)
            )

{- | One @**@, however many imports were listed: @allows-only@ names what may be
imported, and \"everything else\" is a single thing to forbid.

Not transitive. @allows-only@ speaks about what the module itself imports, and a
transitive forbid of everything would be violated by anything any dependency
reached - a rule about other modules' imports, which is not what was asked for.
-}
forbidsEverything :: ForbidsDto
forbidsEverything = ForbidsImportDto {target = GlobDto "**", transitive = Nothing}
