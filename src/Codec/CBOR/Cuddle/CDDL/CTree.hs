{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.CBOR.Cuddle.CDDL.CTree where

import Codec.CBOR.Cuddle.CDDL (
  Name,
  OccurrenceIndicator,
  RangeBound,
  Value,
  XCddl,
  XRule,
  XTerm,
  XXTopLevel,
  XXType2,
 )
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORGenerator, CBORValidator)
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Control.Monad.Identity (Identity (..))
import Data.Default.Class (Default)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- * Resolved CDDL Tree

--
-- This is a simplified representation of CDDL. It is technically more general -
-- that is, the structure can represent invalid CDDL - but is in that way easier
-- to manipulate.
--------------------------------------------------------------------------------

data family XXCTree i

type data CTreePhase

data instance XTerm CTreePhase = CTreeXTerm
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (Hashable, Default)

newtype instance XXTopLevel CTreePhase = CTreeXXTopLevel Void
  deriving (Generic, Show, Eq, Ord)

data instance XCddl CTreePhase = CTreeXCddl
  deriving (Generic, Show, Eq, Ord)

data instance XRule CTreePhase = CTreeXRule (Maybe CBORGenerator) (Maybe CBORValidator)
  deriving (Generic)

newtype instance XXType2 CTreePhase = CTreeXXType2 Void
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (Hashable)

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
  | CTreeE (XXCTree i)
  deriving (Generic)

deriving instance Eq (Node f) => Eq (CTree f)

deriving instance Show (Node f) => Show (CTree f)

instance Hashable (Node f) => Hashable (CTree f)

-- | Traverse the CTree, carrying out the given operation at each node
traverseCTree ::
  Monad m => (XXCTree i -> m (CTree j)) -> (CTree i -> m (CTree j)) -> CTree i -> m (CTree j)
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

foldCTree ::
  (XXCTree i -> CTree j) ->
  (CTree i -> CTree j) ->
  CTree i ->
  CTree j
foldCTree atExt atNode x = runIdentity $ traverseCTree (pure . atExt) (pure . atNode) x

type Node i = XXCTree i

newtype CTreeRoot i = CTreeRoot (Map.Map Name (CTree i))
  deriving (Generic)

deriving instance Show (CTree i) => Show (CTreeRoot i)

-- |
--
--  CDDL predefines a number of names.  This subsection summarizes these
--  names, but please see Appendix D for the exact definitions.
--
--  The following keywords for primitive datatypes are defined:
--
--  "bool"  Boolean value (major type 7, additional information 20
--    or 21).
--
--  "uint"  An unsigned integer (major type 0).
--
--  "nint"  A negative integer (major type 1).
--
--  "int"  An unsigned integer or a negative integer.
--
--  "float16"  A number representable as a half-precision float [IEEE754]
--    (major type 7, additional information 25).
--
--  "float32"  A number representable as a single-precision float
--    [IEEE754] (major type 7, additional information 26).
--
--
--  "float64"  A number representable as a double-precision float
--    [IEEE754] (major type 7, additional information 27).
--
--  "float"  One of float16, float32, or float64.
--
--  "bstr" or "bytes"  A byte string (major type 2).
--
--  "tstr" or "text"  Text string (major type 3).
--
--  (Note that there are no predefined names for arrays or maps; these
--  are defined with the syntax given below.)
data PTerm
  = PTBool
  | PTUInt
  | PTNInt
  | PTInt
  | PTHalf
  | PTFloat
  | PTDouble
  | PTBytes
  | PTText
  | PTAny
  | PTNil
  | PTUndefined
  deriving (Eq, Generic, Ord, Show)

instance Hashable PTerm
