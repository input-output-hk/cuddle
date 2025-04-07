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
import Prettyprinter (Doc, Pretty (..), line', vcat, (<+>))

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
prettyRow = prettyRow'
  where
    prettyRow' [] (Cell c _ : cs) = c <> prettyRow' [] cs
    prettyRow' _ [] = mempty
    prettyRow' (0 : ws) (_ : cs) = prettyRow' ws cs -- Skip empty columns
    prettyRow' (w : ws) (Cell c alignment : cs) =
      let
        align' = case alignment of
          LeftAlign -> fillRight
          RightAlign -> fillLeft
       in
        align' w c <> prettyRow' ws cs

prettyColumnar :: forall ann. Columnar ann -> Doc ann
prettyColumnar (Columnar rows) = vcat $ prettyRow columnWidths . rowCells <$> rows
  where
    columnWidths =
      foldr (zipWith max . fmap (renderedLen . cellDoc) . rowCells) (repeat 0) rows

columnarListing :: Doc ann -> Doc ann -> Doc ann -> Columnar ann -> Doc ann
columnarListing lEnc rEnc _ (Columnar []) = lEnc <> rEnc
columnarListing lEnc rEnc s (Columnar (row : rows)) =
  prettyColumnar
    ( Columnar $
        prependCell (Cell lEnc LeftAlign) row
          : (prependCell (Cell s LeftAlign) <$> rows)
    )
    <> line'
    <> rEnc

columnarSepBy :: Doc ann -> Columnar ann -> Doc ann
columnarSepBy _ (Columnar []) = mempty
columnarSepBy s (Columnar (x : xs)) =
  prettyColumnar (Columnar [x]) <> line' <> prettyColumnar (Columnar $ prependRow <$> xs)
  where
    prependRow (Row (Cell c al : cs)) = Row $ Cell (s <+> c) al : cs
    prependRow (Row []) = Row []
