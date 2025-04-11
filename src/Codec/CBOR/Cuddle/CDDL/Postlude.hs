module Codec.CBOR.Cuddle.CDDL.Postlude where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- |
--
--  CDDL predefines a number of names.  This subsection summarizes these
--  names, but please see Appendix D for the exact definitions.
--
--  The following keywords for primitive datatypes are defined:
--
--  "bool"  Boolean value (major type 7, additional information 20
--    or 21).
--
--  "uint"  An unsigned integer (major type 0).
--
--  "nint"  A negative integer (major type 1).
--
--  "int"  An unsigned integer or a negative integer.
--
--  "float16"  A number representable as a half-precision float [IEEE754]
--    (major type 7, additional information 25).
--
--  "float32"  A number representable as a single-precision float
--    [IEEE754] (major type 7, additional information 26).
--
--
--  "float64"  A number representable as a double-precision float
--    [IEEE754] (major type 7, additional information 27).
--
--  "float"  One of float16, float32, or float64.
--
--  "bstr" or "bytes"  A byte string (major type 2).
--
--  "tstr" or "text"  Text string (major type 3).
--
--  (Note that there are no predefined names for arrays or maps; these
--  are defined with the syntax given below.)
data PTerm
  = PTBool
  | PTUInt
  | PTNInt
  | PTInt
  | PTHalf
  | PTFloat
  | PTDouble
  | PTBytes
  | PTText
  | PTAny
  | PTNil
  | PTUndefined
  deriving (Eq, Generic, Ord, Show)

instance Hashable PTerm
