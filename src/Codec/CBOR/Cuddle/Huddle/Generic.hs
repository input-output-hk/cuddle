module Codec.CBOR.Cuddle.Huddle.Generic where

-- | Function carrying its argument
data FnWithArg a result = FnWithArg
  { fn :: a -> result
  , arg :: a
  }
  deriving (Functor)

-- | Evaluate a function carrying its argument to its result
result :: FnWithArg a result -> result
result a = fn a (arg a)
