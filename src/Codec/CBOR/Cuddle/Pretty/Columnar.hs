{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides some utility functions to help with aligning pretty
-- printed values by column.
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

-- | Creates a cell by pretty printing the input value and then left-aligning
-- the resulting `Doc` within the table.
cellL :: Pretty a => a -> Cell ann
cellL = (`Cell` LeftAlign) . pretty

-- | Creates a cell by pretty printing the input value and then right-aligning
-- the resulting `Doc` within the table.
cellR :: Pretty a => a -> Cell ann
cellR = (`Cell` RightAlign) . pretty

-- | A cell that takes up a cell but has no content.
emptyCell :: Cell ann
emptyCell = Cell mempty LeftAlign

-- | Checks whether the cell contains a `Doc` with a rendered width of zero.
isEmptyCell :: Cell ann -> Bool
isEmptyCell (Cell d _) = renderedLen d == 0

-- | A row within the table.
newtype Row ann = Row {rowCells :: [Cell ann]}

-- | Adds a cell at the beginning of the row.
prependCell :: Cell ann -> Row ann -> Row ann
prependCell c (Row cs) = Row $ c : cs

-- | A row with a single left-aligned document
singletonRow :: Doc ann -> Row ann
singletonRow x = Row [Cell x LeftAlign]

-- | `Columnar` is a two-dimensional table of `Doc`s. When rendered, the cells
-- within each row will be aligned with the cells of every other row in the
-- same column.
newtype Columnar ann = Columnar {colRows :: [Row ann]}

prettyRow :: [Int] -> [Cell ann] -> Doc ann
prettyRow = prettyRow'
  where
    prettyRow' [] (Cell c _ : cs) = c <> prettyRow' [] cs
    prettyRow' _ [] = mempty
    prettyRow' _ [Cell c LeftAlign] = c -- Do not add white space to the last cell
    prettyRow' (0 : ws) (_ : cs) = prettyRow' ws cs -- Skip empty columns
    prettyRow' (w : ws) (Cell c alignment : cs) =
      let
        align' = case alignment of
          LeftAlign -> fillRight
          RightAlign -> fillLeft
       in
        align' w c <> prettyRow' ws cs

-- | Pretty print the `Columnar` as a table.
prettyColumnar :: forall ann. Columnar ann -> Doc ann
prettyColumnar (Columnar rows) = vcat $ prettyRow columnWidths . rowCells <$> rows
  where
    columnWidths =
      foldr (zipWith max . fmap (renderedLen . cellDoc) . rowCells) (repeat 0) rows

-- | Pretty prints the `Columnar` so that the rows are separated by by the
-- separator `Doc` provided as the third argument and then everything is
-- enclosed within the left and right brackets provided as the first and second
-- argument accordingly. The brackets will be aligned with the separators in the
-- first column, e.g.
-- ```
-- [ foo
-- , bar
-- ]
-- ```
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

-- | Pretty prints the `Columnar` so that every line after the first has a
-- separator prepended to it. This can be useful when you want to align the rows,
-- but the separator would cause all the other rows after the first to be shifted
-- right by one. The way you use this is you reduce the indentation on the
-- following lines by the width of the separator.
-- ```
-- foo = x
--     , y
--     , z
-- ```
columnarSepBy :: Doc ann -> Columnar ann -> Doc ann
columnarSepBy _ (Columnar []) = mempty
columnarSepBy s (Columnar rows@(Row x : xs)) =
  prettyRow columnWidths x <> line' <> prettyColumnar (Columnar $ prependRow <$> xs)
  where
    prependRow (Row (Cell c al : cs)) = Row $ Cell (s <+> c) al : cs
    prependRow (Row []) = Row []
    columnWidths =
      foldr (zipWith max . fmap (renderedLen . cellDoc) . rowCells) (repeat 0) rows
