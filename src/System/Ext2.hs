-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2
-- Copyright   : (C) 2014 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : portable
--
-- This library exists for me to learn about ext2 and to possibly be used as
-- part of an assignment in a Computer Science course I will be taking soon.
--
-- Please feel free to use it to learn about ext2 and play around, but don't
-- use it for anything that you care about for now, because I don't trust
-- myself.
----------------------------------------------------------------------------
module System.Ext2 where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BL

data Superblock = Superblock {
    inodesCount     :: Word32
    -- ^ Total number of inodes in the file system
  , blocksCount     :: Word32
    -- ^ Total number of blocks in the file system
  , rBlocksCount    :: Word32
    -- ^ Number of blocks reserved for the superuser
  , freeBlocksCount :: Word32
    -- ^ Number of unallocated blocks
  , freeInodesCount :: Word32
    -- ^ Number of unallocated inodes
  , firstDataBlock  :: Word32
    -- ^ Block number of the block containing the superblock
  , logBlockSize    :: Word32
    -- ^ log2(block size) - 10
  , logFragSize     :: Word32
    -- ^ log2(fragment size) - 10
  , blocksPerGroup  :: Word32
    -- ^ Number of blocks in each group
  , fragsPerGroup   :: Word32
    -- ^ Number of gragments in each group
  , inodesPerGroup  :: Word32
    -- ^ Number of inodes in each group
  , mTime           :: Word32
    -- ^ Last mount time
  , wTime           :: Word32
    -- ^ Last write time
  , mntCount        :: Word16
    -- ^ Number of mounts since last consistency check
  , maxMntCount     :: Word16
    -- ^ Number of allowed mounts before requiring a consistency check
  , magic           :: Word16
    -- ^ ext2 signature: @0xef53@
  , state           :: Word16
    -- ^ Filesystem state
  , errors          :: Word16
    -- ^ What to do on an error condition
  , minorRevLevel   :: Word16
    -- ^ Minor portion of version
  , lastCheck       :: Word32
    -- ^ Time of last consistency check
  , checkInterval   :: Word32
    -- ^ Interval between forced consistency checks
  , creatorOs       :: Word32
    -- ^ Operating system ID
  , revLevel        :: Word32
    -- ^ Major portion of version
  , defResuid       :: Word16
    -- ^ User ID that can use reserved blocks
  , defResgid       :: Word16
    -- ^ Group ID that can use reserved blocks
  } deriving (Eq, Ord, Show)

-- | Reads the superblock information from an ext2 filesystem. __Does not__ skip
-- the first 1024 bytes to where the superblock lives.
readSuperblock :: Get Superblock
readSuperblock = do
  Superblock <$> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord16le
             <*> getWord16le
             <*> getWord16le
             <*> getWord16le
             <*> getWord16le
             <*> getWord16le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord32le
             <*> getWord16le
             <*> getWord16le

-- | Open an ext2 filesystem and parse out the 'Superblock'.
getSuperblock :: String -> IO Superblock
getSuperblock fn = do
  input <- BL.readFile fn
  return $ runGet (skip 1024 >> readSuperblock) input
