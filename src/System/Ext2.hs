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
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BL
import Debug.Trace

data Superblock = Superblock {
    sbInodesCount     :: Word32
    -- ^ Total number of inodes in the file system
  , sbBlocksCount     :: Word32
    -- ^ Total number of blocks in the file system
  , sbRBlocksCount    :: Word32
    -- ^ Number of blocks reserved for the superuser
  , sbFreeBlocksCount :: Word32
    -- ^ Number of unallocated blocks
  , sbFreeInodesCount :: Word32
    -- ^ Number of unallocated inodes
  , sbFirstDataBlock  :: Word32
    -- ^ Block number of the block containing the superblock
  , sbLogBlockSize    :: Word32
    -- ^ log2(block size) - 10
  , sbLogFragSize     :: Word32
    -- ^ log2(fragment size) - 10
  , sbBlocksPerGroup  :: Word32
    -- ^ Number of blocks in each group
  , sbFragsPerGroup   :: Word32
    -- ^ Number of gragments in each group
  , sbInodesPerGroup  :: Word32
    -- ^ Number of inodes in each group
  , sbMTime           :: Word32
    -- ^ Last mount time
  , sbWTime           :: Word32
    -- ^ Last write time
  , sbMntCount        :: Word16
    -- ^ Number of mounts since last consistency check
  , sbMaxMntCount     :: Word16
    -- ^ Number of allowed mounts before requiring a consistency check
  , sbMagic           :: Word16
    -- ^ ext2 signature: @0xef53@
  , sbState           :: Word16
    -- ^ Filesystem state
  , sbErrors          :: Word16
    -- ^ What to do on an error condition
  , sbMinorRevLevel   :: Word16
    -- ^ Minor portion of version
  , sbLastCheck       :: Word32
    -- ^ Time of last consistency check
  , sbCheckInterval   :: Word32
    -- ^ Interval between forced consistency checks
  , sbCreatorOs       :: Word32
    -- ^ Operating system ID
  , sbRevLevel        :: Word32
    -- ^ Major portion of version
  , sbDefResuid       :: Word16
    -- ^ User ID that can use reserved blocks
  , sbDefResgid       :: Word16
    -- ^ Group ID that can use reserved blocks
  } deriving (Eq, Ord, Show)

-- | Reads the superblock information from an ext2 filesystem. __Does not__ skip
-- the first 1024 bytes to where the superblock lives.
--
-- See also 'readExtendedSuperblock'
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

data ExtendedSuperblock = ExtendedSuperblock {
    sbFirstIno          :: Word32
  , sbInodeSize         :: Word16
  , sbBlockGroupNumber  :: Word16
  , sbFeatureCompat     :: Word32
  , sbFeatureIncompat   :: Word32
  , sbFeatureRoCompat   :: Word32
  , sbUuid              :: BL.ByteString
  , sbVolumeName        :: BL.ByteString
  , sbLastMounted       :: BL.ByteString
  , sbAlgoBitmap        :: Word32
  , sbPreallocBlocks    :: Word8
  , sbPreallocDirBlocks :: Word8
  , sbUnusedAlignment   :: Word16
  , sbJournalUuid       :: BL.ByteString
  , sbJournalInum       :: Word32
  , sbJournalDev        :: Word32
  , sbJournalLastOrphan :: Word32
  , sbUnused            :: BL.ByteString
  } deriving (Eq, Ord, Show)

readExtendedSuperblock :: Get ExtendedSuperblock
readExtendedSuperblock = do
  let esb = ExtendedSuperblock <$> getWord32le
                               <*> getWord16le
                               <*> getWord16le
                               <*> getWord32le
                               <*> getWord32le
                               <*> getWord32le
                               <*> getLazyByteString 16
                               <*> getLazyByteString 16
                               <*> getLazyByteString 64
                               <*> getWord32le
                               <*> getWord8
                               <*> getWord8
                               <*> getWord16le
                               <*> getLazyByteString 16
                               <*> getWord32le
                               <*> getWord32le
                               <*> getWord32le
                               <*> getLazyByteString 786
  esb

data BlockGroupDescriptorTable = BlockGroupDescriptorTable {
    bgBlockBitmap :: Word32
  , bgInodeBitmap :: Word32
  , bgInodeTable  :: Word32
  , bgFreeBlocksCount :: Word16
  , bgFreeInodesCount :: Word16
  , bgUsedDirsCount   :: Word16
  } deriving (Eq, Ord, Show)

-- | Reads the block group descriptor table. The last 12 ("reserved") bytes are
-- ignored and skipped over (consumed).
readBlockGroupDescriptorTable :: Get BlockGroupDescriptorTable
readBlockGroupDescriptorTable = do
  let sb = BlockGroupDescriptorTable <$> getWord32le
                                     <*> getWord32le
                                     <*> getWord32le
                                     <*> getWord16le
                                     <*> getWord16le
                                     <*> getWord16le
  sb

-- | Get the number of block groups within the file system.
blockGroupCount :: Superblock -> Int
blockGroupCount sb =
  ceiling $
    (fromIntegral (sbBlocksCount sb) :: Double) / fromIntegral (sbBlocksPerGroup sb)

-- | Given a 'Superblock', and the block number, get the byte number at which
-- it starts.
byteFromBlock :: Superblock -> Int -> Int
byteFromBlock sb x = x * (1024 `shiftL` fromIntegral (sbLogBlockSize sb))

-- | Open an ext2 filesystem and parse out the 'Superblock'.
getSuperblock :: String -> IO Superblock
getSuperblock fn = do
  input <- BL.readFile fn
  return $ runGet (skip 1024 >> readSuperblock) input

-- | Open an ext2 filesystem and parse out the 'BlockGroupDescriptorTable'.
getBGDT :: String -> IO BlockGroupDescriptorTable
getBGDT fn = do
  input <- BL.readFile fn
  return $ runGet (skip 2048 >> readBlockGroupDescriptorTable) input
