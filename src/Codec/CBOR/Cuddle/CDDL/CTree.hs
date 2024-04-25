{-# LANGUAGE DataKinds #-}

module Codec.CBOR.Cuddle.CDDL.CTree where

import Codec.CBOR.Cuddle.CDDL
  ( Name,
    OccurrenceIndicator,
    RangeBound,
    Value,
  )
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)

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
  deriving (Generic)

-- | Traverse the CTree, carrying out the given operation at each node
traverseCTree :: (Monad m) => (Node f -> m (Node g)) -> CTree f -> m (CTree g)
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

type Node f = f (CTree f)

newtype CTreeRoot' poly f
  = CTreeRoot
      (Map.Map Name (poly (Node f)))
  deriving (Generic)

type CTreeRoot f = CTreeRoot' (ParametrisedWith [Name]) f

data ParametrisedWith w a
  = Unparametrised {underlying :: a}
  | Parametrised
      { underlying :: a,
        params :: w
      }
  deriving (Eq, Functor, Generic, Foldable, Traversable, Show)

instance (Hashable w, Hashable a) => Hashable (ParametrisedWith w a)
