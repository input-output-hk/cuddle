{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Codec.CBOR.Cuddle.Comments (
  HasComment (..),
  CollectComments (..),
  hasComment,
  (//-),
  (<*!),
  (!*>),
  WithComment (..),
  (!$>),
  Comment (..),
  unComment,
  withComment,
) where

import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.TreeDiff (ToExpr)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
import Optics.Core (Lens', lens, view, (%~), (&), (.~), (^.))

newtype Comment = Comment T.Text
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (Monoid)
  deriving anyclass (ToExpr, Hashable)

instance Semigroup Comment where
  Comment "" <> x = x
  x <> Comment "" = x
  Comment x <> Comment y = Comment $ x <> "\n" <> y

unComment :: Comment -> [T.Text]
unComment (Comment c) = T.lines c

instance Default Comment where def = mempty

class HasComment a where
  commentL :: Lens' a Comment

class GCollectComments f where
  collectCommentsG :: f a -> [Comment]

instance GCollectComments V1 where
  collectCommentsG = \case {}

instance GCollectComments U1 where
  collectCommentsG U1 = []

instance
  ( GCollectComments a
  , GCollectComments b
  ) =>
  GCollectComments (a :+: b)
  where
  collectCommentsG (L1 x) = collectCommentsG x
  collectCommentsG (R1 x) = collectCommentsG x

instance
  ( GCollectComments a
  , GCollectComments b
  ) =>
  GCollectComments (a :*: b)
  where
  collectCommentsG (a :*: b) = collectCommentsG a <> collectCommentsG b

instance CollectComments a => GCollectComments (K1 s a) where
  collectCommentsG (K1 x) = collectComments x

instance GCollectComments a => GCollectComments (M1 i c a) where
  collectCommentsG (M1 x) = collectCommentsG x

class CollectComments a where
  collectComments :: a -> [Comment]
  default collectComments :: (Generic a, GCollectComments (Rep a)) => a -> [Comment]
  collectComments = collectCommentsG . from

instance CollectComments a => CollectComments (Maybe a) where
  collectComments Nothing = []
  collectComments (Just x) = collectComments x

instance CollectComments a => CollectComments [a]

instance CollectComments a => CollectComments (NonEmpty a)

instance CollectComments Word8 where collectComments = mempty

instance CollectComments Word16 where collectComments = mempty

instance CollectComments Word32 where collectComments = mempty

instance CollectComments Word64 where collectComments = mempty

instance CollectComments Integer where collectComments = mempty

instance CollectComments Float where collectComments = mempty

instance CollectComments Double where collectComments = mempty

instance CollectComments T.Text where collectComments = mempty

instance CollectComments ByteString where collectComments = mempty

instance CollectComments Bool where collectComments = mempty

instance CollectComments Comment where
  collectComments = L.singleton

hasComment :: HasComment a => a -> Bool
hasComment = (/= mempty) . view commentL

(//-) :: HasComment a => a -> Comment -> a
x //- cmt = x & commentL %~ (<> cmt)

infixr 0 //-

(<*!) :: (HasComment a, Applicative m) => m a -> m Comment -> m a
(<*!) = liftA2 (//-)

(!*>) :: (HasComment a, Applicative m) => m Comment -> m a -> m a
(!*>) = liftA2 (flip (//-))

data WithComment a = WithComment
  { unWithComment :: Comment
  , stripComment :: a
  }
  deriving (Functor)

instance Applicative WithComment where
  pure = withComment
  WithComment cmt f <*> WithComment cmt' x = WithComment (cmt <> cmt') $ f x

instance Monad WithComment where
  WithComment cmt x >>= f = let WithComment cmt' y = f x in WithComment (cmt <> cmt') y

instance HasComment (WithComment a) where
  commentL = lens (\(WithComment x _) -> x) (\(WithComment _ b) x -> WithComment x b)

instance CollectComments a => CollectComments (WithComment a) where
  collectComments (WithComment c x) = c : collectComments x

withComment :: a -> WithComment a
withComment = WithComment mempty

(!$>) :: (HasComment b, Functor f) => (a -> b) -> f (WithComment a) -> f b
f !$> wc = fmap (\(WithComment c x) -> f x //- c) wc

instance HasComment a => HasComment (NonEmpty a) where
  commentL = lens (\(x :| _) -> x ^. commentL) (\(x :| xs) y -> (x & commentL .~ y) :| xs)

instance HasComment Comment where
  commentL = lens id const

instance IsString Comment where
  fromString s = Comment $ T.pack s
