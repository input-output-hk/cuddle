{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.CBOR.Cuddle.CDDL.CTree where

import Codec.CBOR.Cuddle.CDDL (
  Name,
  OccurrenceIndicator,
  RangeBound,
  Value,
 )
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- * Resolved CDDL Tree

--
-- This is a simplified representation of CDDL. It is technically more general -
-- that is, the structure can represent invalid CDDL - but is in that way easier
-- to manipulate.
--------------------------------------------------------------------------------

type family CTreeExt i

-- | CDDL Tree, parametrised over a functor
--
--   We principally use this functor to represent references - thus, every 'f a'
--   may be either an a or a reference to another CTree.
data CTree i
  = Literal Value
  | Postlude PTerm
  | Map [CTree i]
  | Array [CTree i]
  | Choice (NE.NonEmpty (CTree i))
  | Group [CTree i]
  | KV {key :: CTree i, value :: CTree i, cut :: Bool}
  | Occur {item :: CTree i, occurs :: OccurrenceIndicator}
  | Range {from :: CTree i, to :: CTree i, inclusive :: RangeBound}
  | Control {op :: CtlOp, target :: CTree i, controller :: CTree i}
  | Enum (CTree i)
  | Unwrap (CTree i)
  | Tag Word64 (CTree i)
  | CTreeE (CTreeExt i)
  deriving (Generic)

deriving instance Eq (Node f) => Eq (CTree f)

-- | Traverse the CTree, carrying out the given operation at each node
traverseCTree ::
  Monad m => (CTreeExt i -> m (CTree j)) -> (CTree i -> m (CTree j)) -> CTree i -> m (CTree j)
traverseCTree _ _ (Literal a) = pure $ Literal a
traverseCTree _ _ (Postlude a) = pure $ Postlude a
traverseCTree _ atNode (Map xs) = Map <$> traverse atNode xs
traverseCTree _ atNode (Array xs) = Array <$> traverse atNode xs
traverseCTree _ atNode (Group xs) = Group <$> traverse atNode xs
traverseCTree _ atNode (Choice xs) = Choice <$> traverse atNode xs
traverseCTree _ atNode (KV k v c) = do
  k' <- atNode k
  v' <- atNode v
  pure $ KV k' v' c
traverseCTree _ atNode (Occur i occ) = flip Occur occ <$> atNode i
traverseCTree _ atNode (Range f t inc) = do
  f' <- atNode f
  t' <- atNode t
  pure $ Range f' t' inc
traverseCTree _ atNode (Control o t c) = do
  t' <- atNode t
  c' <- atNode c
  pure $ Control o t' c'
traverseCTree _ atNode (Enum ref) = Enum <$> atNode ref
traverseCTree _ atNode (Unwrap ref) = Unwrap <$> atNode ref
traverseCTree _ atNode (Tag i ref) = Tag i <$> atNode ref
traverseCTree atExt _ (CTreeE x) = atExt x

type Node i = CTreeExt i

newtype CTreeRoot i = CTreeRoot (Map.Map Name (CTree i))
  deriving (Generic)

deriving instance Show (CTree i) => Show (CTreeRoot i)
