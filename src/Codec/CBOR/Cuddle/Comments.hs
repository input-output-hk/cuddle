{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Hashable (Hashable)
import Data.Text qualified as T
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Optics.Core (Lens', lens, view, (%~), (&), (^.), (.~))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Default.Class (Default (..))
import Data.String (IsString (..))

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

class CollectComments a where
  collectComments :: a -> [Comment]

instance CollectComments a => CollectComments (Maybe a) where
  collectComments Nothing = []
  collectComments (Just x) = collectComments x

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
