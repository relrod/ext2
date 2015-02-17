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
, sbConsistency
, bgdtConsistency
) where

import Control.Lens
import System.Ext2
import System.Ext2.Lens

-- | Given a superblock, ensure that its magic number is as-expected.
sbMagicValid :: Superblock -> Bool
sbMagicValid sb = (sb ^. magic) == 0xEF53

-- | Given two superblocks, ensure they are consistent. We get this for free
-- by deriving Eq.
sbConsistency :: Superblock -> Superblock -> Bool
sbConsistency = (==)

-- | Given two BGDTs, ensure they are consistent. We get this for free
-- by deriving Eq.
bgdtConsistency :: BlockGroupDescriptorTable -> BlockGroupDescriptorTable -> Bool
bgdtConsistency = (==)
