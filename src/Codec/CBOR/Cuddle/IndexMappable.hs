{-# LANGUAGE DefaultSignatures #-}

module Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt) where

import Codec.CBOR.Cuddle.CDDL (
  CDDL (..),
  GenericArg (..),
  GenericParameter (..),
  GenericParameters (..),
  Group (..),
  GroupEntry (..),
  GroupEntryVariant (..),
  GrpChoice (..),
  MemberKey (..),
  Rule (..),
  TopLevel (..),
  Type0 (..),
  Type1 (..),
  Type2 (..),
  TypeOrGroup (..),
  XCddl,
  XRule,
  XTerm,
  XXTopLevel,
  XXType2,
 )
import Codec.CBOR.Cuddle.CDDL.CTreePhase (
  CTreePhase,
  XCddl (..),
  XRule (..),
  XTerm (..),
 )
import Codec.CBOR.Cuddle.Huddle (
  HuddlePhase,
  XCddl (..),
  XRule (..),
  XTerm (..),
  XXTopLevel (..),
  XXType2 (..),
 )
import Codec.CBOR.Cuddle.Parser (
  ParserPhase,
  XCddl (..),
  XTerm (..),
  XXTopLevel (..),
  XXType2 (..),
 )
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (Coercible, coerce)

class IndexMappable f i j where
  mapIndex :: f i -> f j
  default mapIndex :: Coercible (f i) (f j) => f i -> f j
  mapIndex = coerce

mapCDDLDropExt ::
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  , IndexMappable XRule i j
  ) =>
  CDDL i ->
  CDDL j
mapCDDLDropExt (CDDL r tls _) = CDDL (mapIndex r) (foldMap mapTopLevelDropExt tls) []
  where
    mapTopLevelDropExt (TopLevelRule x) = [TopLevelRule $ mapIndex x]
    mapTopLevelDropExt (XXTopLevel _) = []

instance
  ( IndexMappable XCddl i j
  , IndexMappable XXTopLevel i j
  , IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  , IndexMappable XRule i j
  ) =>
  IndexMappable CDDL i j
  where
  mapIndex (CDDL r tls e) = CDDL (mapIndex r) (mapIndex <$> tls) (mapIndex <$> e)

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  , IndexMappable XRule i j
  ) =>
  IndexMappable Rule i j
  where
  mapIndex (Rule n mg a t c) = Rule n (mapIndex <$> mg) a (mapIndex t) (mapIndex c)

instance
  ( IndexMappable XXTopLevel i j
  , IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  , IndexMappable XRule i j
  ) =>
  IndexMappable TopLevel i j
  where
  mapIndex (TopLevelRule r) = TopLevelRule $ mapIndex r
  mapIndex (XXTopLevel e) = XXTopLevel $ mapIndex e

instance IndexMappable XTerm i j => IndexMappable GenericParameter i j where
  mapIndex (GenericParameter n e) = GenericParameter n $ mapIndex e

instance IndexMappable XTerm i j => IndexMappable GenericParameters i j where
  mapIndex (GenericParameters ns) = GenericParameters $ mapIndex <$> ns

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable TypeOrGroup i j
  where
  mapIndex (TOGType t) = TOGType $ mapIndex t
  mapIndex (TOGGroup g) = TOGGroup $ mapIndex g

instance
  ( IndexMappable XTerm i j
  , IndexMappable XXType2 i j
  ) =>
  IndexMappable GroupEntry i j
  where
  mapIndex (GroupEntry mo gev e) = GroupEntry mo (mapIndex gev) (mapIndex e)

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable GroupEntryVariant i j
  where
  mapIndex (GEType mk t) = GEType (mapIndex <$> mk) $ mapIndex t
  mapIndex (GERef n ma) = GERef n (mapIndex <$> ma)
  mapIndex (GEGroup g) = GEGroup (mapIndex g)

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable MemberKey i j
  where
  mapIndex (MKType t) = MKType $ mapIndex t
  mapIndex (MKBareword n) = MKBareword n
  mapIndex (MKValue x) = MKValue x

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable Type0 i j
  where
  mapIndex (Type0 ts) = Type0 $ mapIndex <$> ts

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable Type1 i j
  where
  mapIndex (Type1 t mo e) = Type1 (mapIndex t) (second mapIndex <$> mo) (mapIndex e)

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable Type2 i j
  where
  mapIndex (T2Value v) = T2Value v
  mapIndex (T2Name n mg) = T2Name n (mapIndex <$> mg)
  mapIndex (T2Group t) = T2Group $ mapIndex t
  mapIndex (T2Map g) = T2Map $ mapIndex g
  mapIndex (T2Array a) = T2Array $ mapIndex a
  mapIndex (T2Unwrapped n mg) = T2Unwrapped n (mapIndex <$> mg)
  mapIndex (T2Enum g) = T2Enum $ mapIndex g
  mapIndex (T2EnumRef n mg) = T2EnumRef n (mapIndex <$> mg)
  mapIndex (T2Tag mt t) = T2Tag mt $ mapIndex t
  mapIndex (T2DataItem t mt) = T2DataItem t mt
  mapIndex T2Any = T2Any
  mapIndex (XXType2 e) = XXType2 $ mapIndex e

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable GenericArg i j
  where
  mapIndex (GenericArg g) = GenericArg $ mapIndex <$> g

instance
  ( IndexMappable XTerm i j
  , IndexMappable XXType2 i j
  ) =>
  IndexMappable Group i j
  where
  mapIndex (Group g) = Group $ mapIndex <$> g

instance
  ( IndexMappable XTerm i j
  , IndexMappable XXType2 i j
  ) =>
  IndexMappable GrpChoice i j
  where
  mapIndex (GrpChoice gs e) = GrpChoice (mapIndex <$> gs) $ mapIndex e

-- ParserPhase -> CTreePhase

instance IndexMappable XCddl ParserPhase CTreePhase where
  mapIndex _ = CTreeXCddl

instance IndexMappable XXType2 ParserPhase CTreePhase where
  mapIndex (ParserXXType2 c) = case c of {}

instance IndexMappable XTerm ParserPhase CTreePhase where
  mapIndex _ = CTreeXTerm

instance IndexMappable XRule ParserPhase CTreePhase where
  mapIndex _ = CTreeXRule Nothing Nothing

-- ParserPhase -> HuddlePhase

instance IndexMappable XCddl ParserPhase HuddlePhase where
  mapIndex (ParserXCddl c) = HuddleXCddl c

instance IndexMappable XXTopLevel ParserPhase HuddlePhase where
  mapIndex (ParserXXTopLevel c) = HuddleXXTopLevel c

instance IndexMappable XXType2 ParserPhase HuddlePhase where
  mapIndex (ParserXXType2 c) = HuddleXXType2 c

instance IndexMappable XTerm ParserPhase HuddlePhase where
  mapIndex (ParserXTerm c) = HuddleXTerm c

-- HuddlePhase -> CTreePhase

instance IndexMappable XCddl HuddlePhase CTreePhase where
  mapIndex _ = CTreeXCddl

instance IndexMappable XXType2 HuddlePhase CTreePhase where
  mapIndex (HuddleXXType2 c) = case c of {}

instance IndexMappable XTerm HuddlePhase CTreePhase where
  mapIndex _ = CTreeXTerm

instance IndexMappable XRule HuddlePhase CTreePhase where
  mapIndex (HuddleXRule _ g v) = CTreeXRule g v

-- ParserPhase ~ ParserPhase

instance IndexMappable XCddl ParserPhase ParserPhase

instance IndexMappable XXTopLevel ParserPhase ParserPhase

instance IndexMappable XXType2 ParserPhase ParserPhase

instance IndexMappable XTerm ParserPhase ParserPhase

instance IndexMappable XRule ParserPhase ParserPhase
