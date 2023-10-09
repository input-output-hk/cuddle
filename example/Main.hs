{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.Pretty ()
import Prettyprinter
import Prettyprinter.Util (putDocW)
import qualified Data.List.NonEmpty as NE

ex1 :: Rule
ex1 =
  Rule
    (Name "test1")
    (Just . GenericParam $ NE.singleton (Name "a"))
    AssignEq
    ( TOGType $ mkType (T2Value $ VNum 3) <> mkType  (T2Value $ VText "3")
    )

main :: IO ()
main = putDocW 80 $ pretty ex1
