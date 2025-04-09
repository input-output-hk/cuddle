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
) where

import Prettyprinter (Doc, vsep, Pretty (..), space)
import Codec.CBOR.Cuddle.Pretty.Utils (fillRight, fillLeft, renderedLen)

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
        align = case alignment of
          LeftAlign -> fillRight
          RightAlign -> fillLeft
      in emptyIfFirst isFirst <> align w c <> prettyRow' False ws cs

prettyColumnar :: forall ann. Columnar ann -> Doc ann
prettyColumnar (Columnar rows) = vsep $ prettyRow columnWidths . rowCells <$> rows
  where
    columnWidths =
      foldr (zipWith max . fmap (renderedLen . cellDoc) . rowCells) (repeat 0) rows
