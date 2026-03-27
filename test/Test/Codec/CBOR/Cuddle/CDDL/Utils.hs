module Test.Codec.CBOR.Cuddle.CDDL.Utils (tryResolveHuddle) where

import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (Huddle, toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt)
import GHC.Stack (HasCallStack)
import Test.Hspec.Core.Spec (SpecM, runIO)

tryResolveHuddle :: HasCallStack => Huddle -> SpecM () (CTreeRoot MonoReferenced)
tryResolveHuddle huddle =
  case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> runIO . fail $ "Failed to resolve CDDL:\n" <> show err
