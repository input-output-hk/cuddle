module Codec.CBOR.Cuddle.CDDL.CtlOp where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | A _control_ allows relating a _target_ type with a _controller_ type
--  via a _control operator_.

--  The syntax for a control type is "target .control-operator
--  controller", where control operators are special identifiers prefixed
--  by a dot.  (Note that _target_ or _controller_ might need to be
--  parenthesized.)

--  A number of control operators are defined at this point.  Further
--  control operators may be defined by new versions of this
--  specification or by registering them according to the procedures in
--  Section 6.1.
data CtlOp
  = Size
  | Bits
  | Regexp
  | Cbor
  | Cborseq
  | Within
  | And
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Default
  deriving (Eq, Generic, Show)

instance Hashable CtlOp
