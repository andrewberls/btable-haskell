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
  -- * Re-export modules
  module Data.BTable.Get,
  module Data.BTable.Put
  ) where

import Data.BTable.Get
import Data.BTable.Put
