--------------------------------------------------------------------
-- |
-- Module    : Data.BTable
-- Copyright : (c) Andrew Berls, 2015
-- License   : MIT
--
-- Maintainer:  andrew.berls@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- Binary serialization format for sparse, labeled 2D numeric datasets
--
--------------------------------------------------------------------

module Data.BTable (
  readVersion,
  readLabels,
  readRows
  ) where

import           Data.Binary.Get
import qualified Data.BTable.Get as BGet

readVersion :: Get Int
readVersion = BGet.readVersion

readLabels :: Get [String]
readLabels = BGet.readLabels

readRows :: Get [[Double]]
readRows = BGet.readRows
