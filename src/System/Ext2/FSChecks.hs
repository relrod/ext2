-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2.FSChecks
-- Copyright   : (C) 2014 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens
--
-- This module contains several checks for testing the integrity of an ext2
-- filesystem.
----------------------------------------------------------------------------
module System.Ext2.FSChecks (
  sbMagicValid
) where

import Control.Lens
import System.Ext2

-- | Given a superblock, ensure that its magic number is as-expected.
sbMagicValid :: Superblock -> Bool
sbMagicValid sb = (sb ^. magic) == 0xEF53
