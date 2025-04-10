{-# LANGUAGE ScopedTypeVariables #-}

module Codec.CBOR.Cuddle.Pretty.Columnar (
  CellAlign (..),
  Row (..),
  Cell (..),
  cellL,
  cellR,
  emptyCell,
  isEmptyCell,
  Columnar (..),
  prettyColumnar,
  columnarListing,
  columnarSepBy,
  singletonRow,
) where

import Codec.CBOR.Cuddle.Pretty.Utils (fillLeft, fillRight, renderedLen)
import Prettyprinter (Doc, Pretty (..), space, vcat, (<+>), line)

data CellAlign
  = LeftAlign
  | RightAlign

data Cell ann = Cell
  { cellDoc :: Doc ann
  , cellAlign :: CellAlign
  }

cellL :: Pretty a => a -> Cell ann
cellL = (`Cell` LeftAlign) . pretty

cellR :: Pretty a => a -> Cell ann
cellR = (`Cell` RightAlign) . pretty

emptyCell :: Cell ann
emptyCell = Cell mempty LeftAlign

isEmptyCell :: Cell ann -> Bool
isEmptyCell (Cell d _) = renderedLen d == 0

newtype Row ann = Row {rowCells :: [Cell ann]}

prependCell :: Cell ann -> Row ann -> Row ann
prependCell c (Row cs) = Row $ c : cs

singletonRow :: Doc ann -> Row ann
singletonRow x = Row [Cell x LeftAlign]

newtype Columnar ann = Columnar {colRows :: [Row ann]}

prettyRow :: [Int] -> [Cell ann] -> Doc ann
prettyRow = prettyRow' True
  where
    emptyIfFirst False = space
    emptyIfFirst True = mempty

    prettyRow' isFirst [] (Cell c _ : cs) = emptyIfFirst isFirst <> c <> prettyRow' False [] cs
    prettyRow' _ _ [] = mempty
    prettyRow' isFirst (0 : ws) (_ : cs) = prettyRow' isFirst ws cs -- Skip empty columns
    prettyRow' isFirst (w : ws) (Cell c alignment : cs) =
      let
        align' = case alignment of
          LeftAlign -> fillRight
          RightAlign -> fillLeft
       in
        emptyIfFirst isFirst <> align' w c <> prettyRow' False ws cs

prettyColumnar :: forall ann. Columnar ann -> Doc ann
prettyColumnar (Columnar rows) = vcat $ prettyRow columnWidths . rowCells <$> rows
  where
    columnWidths =
      foldr (zipWith max . fmap (renderedLen . cellDoc) . rowCells) (repeat 0) rows

columnarListing :: Doc ann -> Doc ann -> Doc ann -> Columnar ann -> Doc ann
columnarListing lEnc rEnc _ (Columnar []) = lEnc <> rEnc
columnarListing lEnc rEnc s (Columnar (row : rows)) =
  prettyColumnar . Columnar $
    prependCell (Cell lEnc LeftAlign) row
      : (prependCell (Cell s LeftAlign) <$> rows) <> [Row [Cell rEnc LeftAlign]]

columnarSepBy :: Doc ann -> Columnar ann -> Doc ann
columnarSepBy _ (Columnar []) = mempty
columnarSepBy s (Columnar (x : xs)) = 
  prettyColumnar (Columnar [x]) <> line <> prettyColumnar (Columnar $ prependRow <$> xs)
  where
    prependRow (Row (Cell c al : cs)) = Row $ Cell (s <+> c) al : cs
    prependRow (Row []) = Row []
