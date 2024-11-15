-- | Monad for declaring Huddle constructs
module Codec.CBOR.Cuddle.Huddle.HuddleM
  ( module Huddle,
    (=:=),
    (=:~),
    (=::=),
    binding,
    setRootRules,
    huddleDef,
    huddleDef',
    include,
    unsafeIncludeFromHuddle,
  )
where

import Codec.CBOR.Cuddle.Huddle hiding (binding, (=:=), (=:~))
import Codec.CBOR.Cuddle.Huddle qualified as Huddle
import Control.Monad.State.Strict (State, modify, runState)
import Data.Default.Class (def)
import Data.Generics.Product (HasField (..))
import Data.Map.Ordered.Strict qualified as OMap
import Data.Text qualified as T
import Optics.Core (set, (%~), (^.))

type HuddleM = State Huddle

-- | Overridden version of assignment which also adds the rule to the state
(=:=) :: (IsType0 a) => T.Text -> a -> HuddleM Rule
n =:= b = let r = n Huddle.=:= b in include r

infixl 1 =:=

-- | Overridden version of group assignment which adds the rule to the state
(=:~) :: T.Text -> Group -> HuddleM (Named Group)
n =:~ b = let r = n Huddle.=:~ b in include r

infixl 1 =:~

binding ::
  forall t0.
  (IsType0 t0) =>
  (GRef -> Rule) ->
  HuddleM (t0 -> GRuleCall)
binding fRule = include (Huddle.binding fRule)

-- | Renamed version of Huddle's underlying '=:=' for use in generic bindings
(=::=) :: (IsType0 a) => T.Text -> a -> Rule
n =::= b = n Huddle.=:= b

infixl 1 =::=

setRootRules :: [Rule] -> HuddleM ()
setRootRules = modify . set (field @"roots")

huddleDef :: HuddleM a -> Huddle
huddleDef = snd . huddleDef'

huddleDef' :: HuddleM a -> (a, Huddle)
huddleDef' mh = runState mh def

class Includable a where
  -- | Include a rule, group, or generic rule defined elsewhere
  include :: a -> HuddleM a

instance Includable Rule where
  include r =
    modify (field @"items" %~ (OMap.|> (r ^. field @"name", HIRule r)))
      >> pure r

instance Includable (Named Group) where
  include r =
    modify
      ( (field @"items")
          %~ (OMap.|> (r ^. field @"name", HIGroup r))
      )
      >> pure r

instance (IsType0 t0) => Includable (t0 -> GRuleCall) where
  include gr =
    let fakeT0 = error "Attempting to unwrap fake value in generic call"
        grDef = callToDef <$> gr fakeT0
        n = grDef ^. field @"name"
     in do
          modify (field @"items" %~ (OMap.|> (n, HIGRule grDef)))
          pure gr

instance Includable HuddleItem where
  include x@(HIRule r) = include r >> pure x
  include x@(HIGroup g) = include g >> pure x
  include x@(HIGRule g) =
    let n = g ^. field @"name"
     in do
          modify (field @"items" %~ (OMap.|> (n, x)))
          pure x

unsafeIncludeFromHuddle ::
  Huddle ->
  T.Text -> 
  HuddleM HuddleItem
unsafeIncludeFromHuddle h name =
  let items = h ^. field @"items"
   in case OMap.lookup name items  of
        Just v -> include v
        Nothing -> error $ show name <> " was not found in Huddle spec"
