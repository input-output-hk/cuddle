{-# LANGUAGE DefaultSignatures #-}

module Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..)) where

import Codec.CBOR.Cuddle.CDDL (
  CDDL (..),
  GenericArg (..),
  GenericParam (..),
  Group (..),
  GroupEntry (..),
  GroupEntryVariant (..),
  GrpChoice (..),
  MemberKey (..),
  Name (..),
  Rule (..),
  TopLevel (..),
  Type0 (..),
  Type1 (..),
  Type2 (..),
  TypeOrGroup (..),
  XCddl,
  XTerm,
  XXTopLevel,
  XXType2,
 )
import Codec.CBOR.Cuddle.CDDL.CTree (
  CTreePhase,
  XCddl (..),
  XTerm (..),
  XXTopLevel (..),
  XXType2 (..),
 )
import Codec.CBOR.Cuddle.Huddle (HuddleStage, XCddl (..), XTerm (..), XXTopLevel (..), XXType2 (..))
import Codec.CBOR.Cuddle.Parser (ParserStage, XCddl (..), XTerm (..), XXTopLevel (..), XXType2 (..))
import Codec.CBOR.Cuddle.Pretty (PrettyStage, XCddl (..), XTerm (..), XXTopLevel (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (Coercible, coerce)
import Data.Void (absurd)

class IndexMappable f i j where
  mapIndex :: f i -> f j
  default mapIndex :: Coercible (f i) (f j) => f i -> f j
  mapIndex = coerce

instance
  ( IndexMappable XCddl i j
  , IndexMappable XXTopLevel i j
  , IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable CDDL i j
  where
  mapIndex (CDDL r tls e) = CDDL (mapIndex r) (mapIndex <$> tls) (mapIndex <$> e)

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable Rule i j
  where
  mapIndex (Rule n mg a t c) = Rule (mapIndex n) (mapIndex <$> mg) a (mapIndex t) (mapIndex c)

instance
  ( IndexMappable XXTopLevel i j
  , IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable TopLevel i j
  where
  mapIndex (TopLevelRule r) = TopLevelRule $ mapIndex r
  mapIndex (XXTopLevel e) = XXTopLevel $ mapIndex e

instance IndexMappable XTerm i j => IndexMappable Name i j where
  mapIndex (Name n e) = Name n $ mapIndex e

instance IndexMappable XTerm i j => IndexMappable GenericParam i j where
  mapIndex (GenericParam ns) = GenericParam $ mapIndex <$> ns

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
  mapIndex (GERef n ma) = GERef (mapIndex n) (mapIndex <$> ma)
  mapIndex (GEGroup g) = GEGroup (mapIndex g)

instance
  ( IndexMappable XXType2 i j
  , IndexMappable XTerm i j
  ) =>
  IndexMappable MemberKey i j
  where
  mapIndex (MKType t) = MKType $ mapIndex t
  mapIndex (MKBareword n) = MKBareword $ mapIndex n
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
  mapIndex (T2Name n mg) = T2Name (mapIndex n) (mapIndex <$> mg)
  mapIndex (T2Group t) = T2Group $ mapIndex t
  mapIndex (T2Map g) = T2Map $ mapIndex g
  mapIndex (T2Array a) = T2Array $ mapIndex a
  mapIndex (T2Unwrapped n mg) = T2Unwrapped (mapIndex n) (mapIndex <$> mg)
  mapIndex (T2Enum g) = T2Enum $ mapIndex g
  mapIndex (T2EnumRef n mg) = T2EnumRef (mapIndex n) (mapIndex <$> mg)
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

-- ParserStage -> PrettyStage

instance IndexMappable XCddl ParserStage PrettyStage where
  mapIndex (ParserXCddl c) = PrettyXCddl c

instance IndexMappable XTerm ParserStage PrettyStage where
  mapIndex (ParserXTerm c) = PrettyXTerm c

instance IndexMappable XXType2 ParserStage PrettyStage where
  mapIndex (ParserXXType2 v) = absurd v

instance IndexMappable XXTopLevel ParserStage PrettyStage where
  mapIndex (ParserXXTopLevel c) = PrettyXXTopLevel c

-- ParserStage -> CTreePhase

instance IndexMappable XCddl ParserStage CTreePhase where
  mapIndex (ParserXCddl c) = CTreeXCddl c

instance IndexMappable XXTopLevel ParserStage CTreePhase where
  mapIndex (ParserXXTopLevel c) = CTreeXXTopLevel c

instance IndexMappable XXType2 ParserStage CTreePhase where
  mapIndex (ParserXXType2 c) = CTreeXXType2 c

instance IndexMappable XTerm ParserStage CTreePhase where
  mapIndex (ParserXTerm c) = CTreeXTerm c

-- ParserStage -> HuddleStage

instance IndexMappable XCddl ParserStage HuddleStage where
  mapIndex (ParserXCddl c) = HuddleXCddl c

instance IndexMappable XXTopLevel ParserStage HuddleStage where
  mapIndex (ParserXXTopLevel c) = HuddleXXTopLevel c

instance IndexMappable XXType2 ParserStage HuddleStage where
  mapIndex (ParserXXType2 c) = HuddleXXType2 c

instance IndexMappable XTerm ParserStage HuddleStage where
  mapIndex (ParserXTerm c) = HuddleXTerm c

-- ParserStage -> ParserStage

instance IndexMappable XCddl ParserStage ParserStage

instance IndexMappable XXTopLevel ParserStage ParserStage

instance IndexMappable XXType2 ParserStage ParserStage

instance IndexMappable XTerm ParserStage ParserStage
