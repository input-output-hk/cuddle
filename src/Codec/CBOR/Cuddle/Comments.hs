{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Codec.CBOR.Cuddle.Comments (
  comment,
  HasComment (..),
  hasComment,
  (//-),
  (<*!),
  (!*>),
  WithComment (..),
  (!$>),
  Comment (..),
  withComment,
) where

import Data.Hashable (Hashable)
import Data.Text qualified as T
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Optics.Core (Lens', lens, view, (%~), (&), (^.), (.~))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Default.Class (Default)

newtype Comment = Comment {unComment :: [T.Text]}
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (Semigroup, Monoid, Default)
  deriving anyclass (ToExpr, Hashable)

comment :: T.Text -> Comment
comment t = Comment [t]

class HasComment a where
  commentL :: Lens' a Comment

hasComment :: HasComment a => a -> Bool
hasComment = (/= mempty) . view commentL

(//-) :: HasComment a => a -> Comment -> a
x //- cmt = x & commentL %~ (<> cmt)

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

withComment :: a -> WithComment a
withComment = WithComment mempty

(!$>) :: (HasComment b, Functor f) => (a -> b) -> f (WithComment a) -> f b
f !$> wc = fmap (\(WithComment c x) -> f x //- c) wc

instance HasComment a => HasComment (NonEmpty a) where
  commentL = lens (\(x :| _) -> x ^. commentL) (\(x :| xs) y -> (x & commentL .~ y) :| xs)

instance HasComment Comment where
  commentL = lens id const
