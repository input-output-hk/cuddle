{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CBOR.Validate where

import Codec.CBOR.Cuddle.Huddle
import Test.Hspec (Spec)

simpleSchema :: Huddle
simpleSchema = ["port" =:= VUInt]

validateSpec :: Spec
validateSpec = undefined
