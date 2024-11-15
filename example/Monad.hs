{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Monad where

import Codec.CBOR.Cuddle.Huddle qualified as Huddle
import Codec.CBOR.Cuddle.Huddle.HuddleM
import Data.Word (Word64)

hdl_set :: (IsType0 t0) => t0 -> GRuleCall
hdl_set = Huddle.binding $ \x -> "set" Huddle.=:= arr [0 <+ a x]

spec :: Huddle
spec = huddleDef $ mdo
  transaction <-
    "transaction"
      =:= mp
        [ idx 0 ==> set txIn,
          idx 1 ==> set txOut
        ]
  txIn <- "txIn" =:= arr ["transaction_id" ==> hash32, "index" ==> txId]
  txOut <- "txOut" =:= arr [idx 0 ==> address, idx 1 ==> value]
  txId <- "txId" =:= VUInt `sized` (2 :: Word64)
  address <- "address" =:= VBytes `sized` (32 :: Word64)
  hash32 <- "hash32" =:= VBytes `sized` (32 :: Word64)
  value <- "value" =:= VUInt
  set <- include hdl_set

  setRootRules [transaction]

spec2 :: Huddle
spec2 =
  spec
    <> huddleDef
      ( mdo
          set <- include hdl_set
          txIn <- unsafeIncludeFromHuddle spec "txIn"
          txOut <- unsafeIncludeFromHuddle spec "txOut"
          _transaction <-
            "transaction"
              =:= mp
                [ idx 0 ==> set txIn,
                  idx 1 ==> set txOut,
                  idx 2 ==> metadata
                ]
          metadata <- "metadata" =:= VBytes
          _value <- "value" =:= mp ["token" ==> VText, "quantity" ==> VUInt]
          pure ()
      )
