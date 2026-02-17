{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Examples.Huddle (
  huddleRangeArray,
  huddleArray,
  huddleMap,
  huddleRangeMap,
  simpleRule,
  simpleTermExample,
  refTermExample,
  bytesExample,
  opCertExample,
  sizeTextExample,
  sizeBytesExample,
  rangeListExample,
  rangeMapExample,
  optionalMapExample,
  choicesExample,
) where

import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.Huddle (
  CanQuantify (..),
  Huddle,
  HuddleItem (..),
  Rule,
  Value (..),
  a,
  arr,
  asKey,
  collectFrom,
  idx,
  mp,
  opt,
  sized,
  withGenerator,
  (...),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Term qualified as C
import Data.Word (Word64)
import Test.QuickCheck.Gen (choose)

huddleRangeArray :: Huddle
huddleRangeArray =
  collectFrom
    [ HIRule $
        "a"
          =:= arr
            [ opt $ a VInt
            , 2 <+ a VInt +> 3
            , a VBool +> 3
            , 3 <+ a VText
            ]
    ]

huddleArray :: Huddle
huddleArray =
  collectFrom
    [ HIRule $
        "a"
          =:= arr
            [ 0 <+ a VBool
            , 1 <+ a VInt
            , opt $ a VText
            , a VUInt
            ]
    ]

huddleMap :: Huddle
huddleMap =
  collectFrom
    [ HIRule $
        "a"
          =:= mp
            [ idx 1 ==> arr [0 <+ a VUInt]
            , 1 <+ asKey VBytes ==> VAny
            , opt $ idx 2 ==> VBool
            , 0 <+ asKey VText ==> VInt
            ]
    ]

huddleRangeMap :: Huddle
huddleRangeMap =
  collectFrom
    [ HIRule $
        "a"
          =:= mp
            [ 5 <+ asKey VInt ==> VBool +> 10
            ]
    ]

simpleRule :: Name -> Rule
simpleRule n = withGenerator (S . C.TInt <$> choose (4, 6)) $ n =:= arr [1, 2, 3]

simpleTermExample :: Huddle
simpleTermExample =
  collectFrom
    [ HIRule $ simpleRule "root"
    ]

refTermExample :: Huddle
refTermExample =
  collectFrom
    [ HIRule $ "root" =:= arr [0, a $ simpleRule "bar"]
    ]

bytesExample :: Huddle
bytesExample =
  collectFrom
    [ HIRule $ "root" =:= H.bstr "010203ff"
    ]

opCertExample :: Huddle
opCertExample =
  collectFrom
    [ HIRule $
        "root"
          =:= arr
            [ a (VBytes `sized` (32 :: Word64))
            , a VUInt
            , a VUInt
            , a (VBytes `sized` (64 :: Word64))
            ]
    ]

sizeTextExample :: Huddle
sizeTextExample =
  collectFrom
    [HIRule $ "root" =:= VText `sized` (0 :: Word64, 32 :: Word64)]

sizeBytesExample :: Huddle
sizeBytesExample =
  collectFrom
    [HIRule $ "root" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)]

rangeListExample :: Huddle
rangeListExample =
  collectFrom
    [ HIRule $
        "root"
          =:= arr
            [ 3 <+ a VInt +> 7
            ]
    ]

rangeMapExample :: Huddle
rangeMapExample =
  collectFrom
    [ HIRule $
        "root"
          =:= mp
            [ 3 <+ asKey VInt ==> VBool +> 7
            ]
    ]

optionalMapExample :: Huddle
optionalMapExample =
  collectFrom
    [ HIRule $
        "root"
          =:= mp
            [ 10 <+ asKey ((0 :: Integer) ... (10 :: Integer)) ==> VBool
            ]
    ]

choicesExample :: Huddle
choicesExample =
  collectFrom
    [ HIRule $
        "root"
          =:= arr [1, a VInt, 3]
          H./ arr [1, a VBool, 6]
          H./ arr [1, a VText]
    ]
