{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Examples.Huddle (
  huddleRangeArray,
  huddleArray,
  huddleMap,
  huddleRangeMap,
  customGenRule,
  customGenExample,
  refTermExample,
  bytesExample,
  opCertExample,
  sizeTextExample,
  sizeBytesExample,
  rangeListExample,
  rangeMapExample,
  optionalMapExample,
  choicesExample,
  cborControlExample,
  listTooShortExample,
  listSkippedRuleExample,
  listSkippedRuleNestedExample,
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
  withCBORGen,
  (...),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Term qualified as C
import Data.Word (Word64)
import Test.QuickCheck.GenT (MonadGen (..))

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
simpleRule n = n =:= arr [1, 2, 3]

customGenRule :: Name -> Rule
customGenRule = withCBORGen (S . C.TInt <$> choose (4, 6)) . simpleRule

customGenExample :: Huddle
customGenExample =
  collectFrom
    [ HIRule $ customGenRule "root"
    ]

refTermExample :: Huddle
refTermExample =
  collectFrom
    [ HIRule $ "root" =:= arr [0, a $ customGenRule "bar"]
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
            [ a (VBytes `H.sized` (32 :: Word64))
            , a VUInt
            , a VUInt
            , a (VBytes `H.sized` (64 :: Word64))
            ]
    ]

sizeTextExample :: Huddle
sizeTextExample =
  collectFrom
    [HIRule $ "root" =:= VText `H.sized` (0 :: Word64, 32 :: Word64)]

sizeBytesExample :: Huddle
sizeBytesExample =
  collectFrom
    [HIRule $ "root" =:= VBytes `H.sized` (0 :: Word64, 32 :: Word64)]

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
            [ 10 <+ asKey ((1 :: Integer) ... (10 :: Integer)) ==> VBool
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

cborControlExample :: Huddle
cborControlExample =
  collectFrom
    [ HIRule $
        "root"
          =:= VBytes
          `H.cbor` simpleRule "simpleRule"
    ]

listTooShortExample :: Huddle
listTooShortExample =
  collectFrom
    [ HIRule $
        "root"
          =:= arr
            [ a VInt
            , a VText
            , a VBool
            ]
    ]

listSkippedRuleExample :: Huddle
listSkippedRuleExample =
  let bar = "bar" =:= VText
   in collectFrom
        [ HIRule $
            "root"
              =:= arr
                [ a VInt
                , opt $ a bar
                ]
        , HIRule bar
        ]

listSkippedRuleNestedExample :: Huddle
listSkippedRuleNestedExample =
  let baz = "baz" =:= arr [a VInt, a VText]
      qux = "qux" =:= VText
      quux = "quux" =:= VInt
   in collectFrom
        [ HIRule $
            "root"
              =:= arr
                [ a VInt
                , opt $ a qux
                , opt $ a baz
                , opt $ a quux
                ]
        , HIRule baz
        , HIRule qux
        , HIRule quux
        ]
