{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Monad where 

import Codec.CBOR.Cuddle.Huddle.HuddleM
import Data.Word (Word64)

spec :: Huddle
spec = huddleDef $ mdo 
  transaction <- "transaction" =:= mp 
    [ idx 0 ==> set txIn,
      idx 1 ==> set txOut
    ]
  txIn <- "txIn" =:= arr [ "transaction_id" ==> hash32, "index" ==> txId]
  txOut <- "txOut" =:= arr [ idx 0 ==> address, idx 1 ==> value]
  txId <- "txId" =:= VUInt `sized` (2 :: Word64)
  address <- "address" =:= VBytes `sized` (32 :: Word64)
  hash32 <- "hash32" =:= VBytes `sized` (32 :: Word64)
  value <- "value" =:= VUInt 
  set <- binding $ \x -> "set" =::= arr [0 <+ a x]

  setRootRules [transaction]
  pure ()
