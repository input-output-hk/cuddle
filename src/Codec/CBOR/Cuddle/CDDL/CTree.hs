{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Codec.CBOR.Cuddle.CDDL.CTree where

import Codec.CBOR.Cuddle.CDDL (
  Name,
  OccurrenceIndicator,
  RangeBound,
  Value,
 )
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm)
import Codec.CBOR.Term (Term)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random.Stateful (StatefulGen)

--------------------------------------------------------------------------------
-- Kinds of terms
--------------------------------------------------------------------------------

data WrappedTerm
  = SingleTerm Term
  | PairTerm Term Term
  | GroupTerm [WrappedTerm]
  deriving (Eq, Show, Generic)

-- | Recursively flatten wrapped list. That is, expand any groups out to their
-- individual entries.
flattenWrappedList :: [WrappedTerm] -> [WrappedTerm]
flattenWrappedList [] = []
flattenWrappedList (GroupTerm xxs : xs) =
  flattenWrappedList xxs <> flattenWrappedList xs
flattenWrappedList (y : xs) = y : flattenWrappedList xs

pattern S :: Term -> WrappedTerm
pattern S t = SingleTerm t

-- | Convert a list of wrapped terms to a list of terms. If any 'PairTerm's are
-- present, we just take their "value" part.
singleTermList :: [WrappedTerm] -> Maybe [Term]
singleTermList [] = Just []
singleTermList (S x : xs) = (x :) <$> singleTermList xs
singleTermList (P _ y : xs) = (y :) <$> singleTermList xs
singleTermList _ = Nothing

pattern P :: Term -> Term -> WrappedTerm
pattern P t1 t2 = PairTerm t1 t2

-- | Convert a list of wrapped terms to a list of pairs of terms, or fail if any
-- 'SingleTerm's are present.
pairTermList :: [WrappedTerm] -> Maybe [(Term, Term)]
pairTermList [] = Just []
pairTermList (P x y : xs) = ((x, y) :) <$> pairTermList xs
pairTermList _ = Nothing

pattern G :: [WrappedTerm] -> WrappedTerm
pattern G xs = GroupTerm xs

--------------------------------------------------------------------------------

-- * Resolved CDDL Tree

--
-- This is a simplified representation of CDDL. It is technically more general -
-- that is, the structure can represent invalid CDDL - but is in that way easier
-- to manipulate.
--------------------------------------------------------------------------------

-- | CDDL Tree, parametrised over a functor
--
--   We principally use this functor to represent references - thus, every 'f a'
--   may be either an a or a reference to another CTree.
data CTree f
  = Literal Value
  | Postlude PTerm
  | Map [Node f]
  | Array [Node f]
  | Choice (NE.NonEmpty (Node f))
  | Group [Node f]
  | KV {key :: Node f, value :: Node f, cut :: Bool}
  | Occur {item :: Node f, occurs :: OccurrenceIndicator}
  | Range {from :: Node f, to :: Node f, inclusive :: RangeBound}
  | Control {op :: CtlOp, target :: Node f, controller :: Node f}
  | Enum (Node f)
  | Unwrap (Node f)
  | Tag Word64 (Node f)
  | WithGen (forall g m. StatefulGen g m => g -> m WrappedTerm) (Node f)

-- | Traverse the CTree, carrying out the given operation at each node
traverseCTree :: Monad m => (Node f -> m (Node g)) -> CTree f -> m (CTree g)
traverseCTree _ (Literal a) = pure $ Literal a
traverseCTree _ (Postlude a) = pure $ Postlude a
traverseCTree atNode (Map xs) = Map <$> traverse atNode xs
traverseCTree atNode (Array xs) = Array <$> traverse atNode xs
traverseCTree atNode (Group xs) = Group <$> traverse atNode xs
traverseCTree atNode (Choice xs) = Choice <$> traverse atNode xs
traverseCTree atNode (KV k v c) = do
  k' <- atNode k
  v' <- atNode v
  pure $ KV k' v' c
traverseCTree atNode (Occur i occ) = flip Occur occ <$> atNode i
traverseCTree atNode (Range f t inc) = do
  f' <- atNode f
  t' <- atNode t
  pure $ Range f' t' inc
traverseCTree atNode (Control o t c) = do
  t' <- atNode t
  c' <- atNode c
  pure $ Control o t' c'
traverseCTree atNode (Enum ref) = Enum <$> atNode ref
traverseCTree atNode (Unwrap ref) = Unwrap <$> atNode ref
traverseCTree atNode (Tag i ref) = Tag i <$> atNode ref
traverseCTree atNode (WithGen g ref) = WithGen g <$> atNode ref

type Node f = f (CTree f)

newtype CTreeRoot' poly f
  = CTreeRoot
      (Map.Map Name (poly (Node f)))
  deriving (Generic)

type CTreeRoot f = CTreeRoot' (ParametrisedWith [Name]) f

data ParametrisedWith w a
  = Unparametrised {underlying :: a}
  | Parametrised
      { underlying :: a
      , params :: w
      }
  deriving (Eq, Functor, Generic, Foldable, Traversable, Show)

instance (Hashable w, Hashable a) => Hashable (ParametrisedWith w a)
