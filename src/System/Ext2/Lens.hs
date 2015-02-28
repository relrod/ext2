{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2.Lens
-- Copyright   : (C) 2015 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens
--
-- This module exports lenses for types in 'System.Ext2'.
----------------------------------------------------------------------------
module System.Ext2.Lens (
    -- * Superblock
    wTime, state, revLevel, rBlocksCount, mntCount, minorRevLevel, maxMntCount
  , magic, mTime, logFragSize, logBlockSize, lastCheck, inodesPerGroup
  , inodesCount, freeInodesCount, freeBlocksCount, fragsPerGroup, firstDataBlock
  , errors, defResuid, defResgid, creatorOs, checkInterval, blocksPerGroup
  , blocksCount, volumeName, uuid, unusedAlignment, preallocDirBlocks
  , preallocBlocks, lastMounted, journalUuid, journalLastOrphan, journalInum
  , journalDev, inodeSize, firstIno, featureRoCompat, featureIncompat
  , featureCompat, blockGroupNumber, algoBitmap, defaultMountOptions
  , defHashVersion, firstMetaBg, hashSeed

    -- * BlockGroupDescriptorTable
  , usedDirsCount, inodeTable, inodeBitmap, freeInodesCountBg, freeBlocksCountBg
  , blockBitmap

    -- * Inode
  , uid, size, osd2, osd1, mtime, mode, linksCount, gid, generation, flags
  , fileAcl, faddr, dtime, dirAcl, ctime, blocks, block, atime

    -- * Directory
  , recLen, nameLen, name, inode, fileType, padding
  ) where

import Control.Lens
import System.Ext2.Tables
import System.Ext2.Internal.LensHacks

makeLensesWith namespaceLensRules ''Superblock

makeLensesWith namespaceLensRules ''BlockGroupDescriptorTable

makeLensesWith namespaceLensRules ''Inode

makeLensesWith namespaceLensRules ''Directory
