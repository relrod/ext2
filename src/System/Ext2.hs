{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2
-- Copyright   : (C) 2014 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens
--
-- This library exists for me to learn about ext2 and to possibly be used as
-- part of an assignment in a Computer Science course I will be taking soon.
--
-- Please feel free to use it to learn about ext2 and play around, but don't
-- use it for anything that you care about for now, because I don't trust
-- myself.
----------------------------------------------------------------------------
module System.Ext2 (
    -- * Superblock
    Superblock (..)
    -- ** Lenses
  , wTime, state, revLevel, rBlocksCount, mntCount, minorRevLevel, maxMntCount
  , magic, mTime, logFragSize, logBlockSize, lastCheck, inodesPerGroup
  , inodesCount, freeInodesCount, freeBlocksCount, fragsPerGroup, firstDataBlock
  , errors, defResuid, defResgid, creatorOs, checkInterval, blocksPerGroup
  , blocksCount, volumeName, uuid, unusedAlignment, preallocDirBlocks
  , preallocBlocks, lastMounted, journalUuid, journalLastOrphan, journalInum
  , journalDev, inodeSize, firstIno, featureRoCompat, featureIncompat
  , featureCompat, blockGroupNumber, algoBitmap, defaultMountOptions
  , defHashVersion, firstMetaBg, hashSeed

    -- ** Parsers
  , readSuperblock

    -- ** Lenses


    -- * BlockGroupDescriptorTable
  , BlockGroupDescriptorTable (..)
    -- ** Lenses
  , usedDirsCount, inodeTable, inodeBitmap, freeInodesCountBg, freeBlocksCountBg
  , blockBitmap
    -- ** Parsers
  , readBlockGroupDescriptorTable

    -- * Inode
  , Inode (..)
    -- ** Lenses
  , uid, size, osd2, osd1, mtime, mode, linksCount, gid, generation, flags
  , fileAcl, faddr, dtime, dirAcl, ctime, blocks, block, atime
    -- ** Parsers
  , readInode
  , readInodeTable

    -- * Directory
  , Directory (..)
    -- ** Lenses
  , recLen, nameLen, name, inode, fileType, padding
    -- ** Parsers
  , readDirectory

    -- * Utility functions
  , blockGroupCount
  , byteFromBlock

    -- * Silly test/helper functions
  , getSuperblock
  , getBGDT
  , getAllTables
  , listRootFiles

  ) where

import Control.Applicative
import Control.Lens
import Data.Bytes.Get
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import qualified Data.Vector as V
import System.Ext2.Internal.LensHacks

data Superblock = Superblock {
    sbInodesCount       :: Word32
    -- ^ Total number of inodes in the file system
  , sbBlocksCount       :: Word32
    -- ^ Total number of blocks in the file system
  , sbRBlocksCount      :: Word32
    -- ^ Number of blocks reserved for the superuser
  , sbFreeBlocksCount   :: Word32
    -- ^ Number of unallocated blocks
  , sbFreeInodesCount   :: Word32
    -- ^ Number of unallocated inodes
  , sbFirstDataBlock    :: Word32
    -- ^ Block number of the block containing the superblock
  , sbLogBlockSize      :: Word32
    -- ^ log2(block size) - 10
  , sbLogFragSize       :: Word32
    -- ^ log2(fragment size) - 10
  , sbBlocksPerGroup    :: Word32
    -- ^ Number of blocks in each group
  , sbFragsPerGroup     :: Word32
    -- ^ Number of gragments in each group
  , sbInodesPerGroup    :: Word32
    -- ^ Number of inodes in each group
  , sbMTime             :: Word32
    -- ^ Last mount time
  , sbWTime             :: Word32
    -- ^ Last write time
  , sbMntCount          :: Word16
    -- ^ Number of mounts since last consistency check
  , sbMaxMntCount       :: Word16
    -- ^ Number of allowed mounts before requiring a consistency check
  , sbMagic             :: Word16
    -- ^ ext2 signature: @0xef53@
  , sbState             :: Word16
    -- ^ Filesystem state
  , sbErrors            :: Word16
    -- ^ What to do on an error condition
  , sbMinorRevLevel     :: Word16
    -- ^ Minor portion of version
  , sbLastCheck         :: Word32
    -- ^ Time of last consistency check
  , sbCheckInterval     :: Word32
    -- ^ Interval between forced consistency checks
  , sbCreatorOs         :: Word32
    -- ^ Operating system ID
  , sbRevLevel          :: Word32
    -- ^ Major portion of version
  , sbDefResuid         :: Word16
    -- ^ User ID that can use reserved blocks
  , sbDefResgid         :: Word16
    -- ^ Group ID that can use reserved blocks
  , sbFirstIno          :: Word32
    -- ^ First non-reserved inode
  , sbInodeSize         :: Word16
    -- ^ Size of each inode structure
  , sbBlockGroupNumber  :: Word16
    -- ^ Block group of this particular superblock (if it is a copy)
  , sbFeatureCompat     :: Word32
    -- ^ Optional features present
  , sbFeatureIncompat   :: Word32
    -- ^ Required features present
  , sbFeatureRoCompat   :: Word32
    -- ^ Features which, if not present, force read-only mounting
  , sbUuid              :: BL.ByteString
    -- ^ File system UUID
  , sbVolumeName        :: BL.ByteString
    -- ^ Volume name
  , sbLastMounted       :: BL.ByteString
    -- ^ Last mounted path
  , sbAlgoBitmap        :: Word32
    -- ^ Compression algorithms used
  , sbPreallocBlocks    :: Word8
    -- ^ Number of blocks to pre-allocate for files
  , sbPreallocDirBlocks :: Word8
    -- ^ Number of blocks to pre-allocate for directories
  , sbUnusedAlignment   :: Word16
    -- ^ UNUSED
  , sbJournalUuid       :: BL.ByteString
    -- ^ Journal UUID
  , sbJournalInum       :: Word32
    -- ^ Journal inode
  , sbJournalDev        :: Word32
    -- ^ Journal device
  , sbJournalLastOrphan :: Word32
    -- ^ Head of orphan inode list
  , sbHashSeed          :: (Word32, Word32, Word32, Word32)
    -- ^ Seeds for hashing algorithm for directory indexing
  , sbDefHashVersion    :: Word8
    -- ^ Default hash version used for directory indexing
  , sbDefaultMountOptions :: Word32
    -- ^ Default mount options for the filesystem
  , sbFirstMetaBg       :: Word32
    -- ^ Block group ID of the first meta block group
  } deriving (Eq, Ord, Show)

makeLensesWith namespaceLensRules ''Superblock

-- | Reads the superblock information from an ext2 filesystem. __Does not__ skip
-- the first 1024 bytes to where the superblock lives.
--
-- See also 'readExtendedSuperblock'
readSuperblock :: MonadGet m => m Superblock
readSuperblock =
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
             <*> getWord32le
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
             <*> liftA4 (,,,)
                 getWord32le
                 getWord32le
                 getWord32le
                 getWord32le
             <*> getWord8
             <*> getWord32le
             <*> getWord32le
  where
    liftA4 fn a b c d =
      fn <$> a <*> b <*> c <*> d


data BlockGroupDescriptorTable = BlockGroupDescriptorTable {
    bgBlockBitmap :: Word32
  , bgInodeBitmap :: Word32
  , bgInodeTable  :: Word32
  , bgFreeBlocksCountBg :: Word16
  , bgFreeInodesCountBg :: Word16
  , bgUsedDirsCount   :: Word16
  } deriving (Eq, Ord, Show)

makeLensesWith namespaceLensRules ''BlockGroupDescriptorTable

-- | Reads the block group descriptor table. The last 12 ("reserved") bytes are
-- ignored and skipped over (consumed).
readBlockGroupDescriptorTable :: MonadGet m => m BlockGroupDescriptorTable
readBlockGroupDescriptorTable =
  BlockGroupDescriptorTable <$> getWord32le
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord16le
                            <*> getWord16le
                            <*> getWord16le

data Inode = Inode {
    iMode :: Word16
  , iUid :: Word16
  , iSize :: Word32
  , iAtime :: Word32
  , iCtime :: Word32
  , iMtime :: Word32
  , iDtime :: Word32
  , iGid :: Word16
  , iLinksCount :: Word16
  , iBlocks :: Word32
  , iFlags :: Word32
  , iOsd1 :: Word32
  , iBlock :: (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)
  , iGeneration :: Word32
  , iFileAcl :: Word32
  , iDirAcl :: Word32
  , iFaddr :: Word32
  , iOsd2 :: BL.ByteString -- TODO: Use a more appropriate type here.
  } deriving (Eq, Ord, Show)

makeLensesWith namespaceLensRules ''Inode

readInode :: MonadGet m => m Inode
readInode =
  Inode <$> getWord16le
        <*> getWord16le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord16le
        <*> getWord16le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> liftA15 (,,,,,,,,,,,,,,)
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
            getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getLazyByteString 12
  where
    -- Holy hell, what could possibly go wrong?
    liftA15 fn a b c d e f g h i j k l m n o =
      fn <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l
         <*> m <*> n <*> o

readInodeTable :: MonadGet m => Int ->  m (V.Vector Inode)
readInodeTable n = V.replicateM n readInode

data Directory = Directory {
    dInode :: Word32
  , dRecLen :: Word16
  , dNameLen :: Word8
  , dFileType :: Word8
  , dName :: BL.ByteString
  , dPadding :: BL.ByteString
  } deriving (Eq, Ord, Show)

makeLensesWith namespaceLensRules ''Directory

readDirectory :: MonadGet m => m Directory
readDirectory = do
  inode' <- getWord32le
  recLen' <- getWord16le
  nameLen' <- getWord8
  fileType' <- getWord8
  name' <- getLazyByteString (fromIntegral nameLen')
  padding' <- getLazyByteString 3
  return $ Directory inode' recLen' nameLen' fileType' name' padding'

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
  return $ runGetL (skip 1024 >> readSuperblock) input

-- | Open an ext2 filesystem and parse out the 'BlockGroupDescriptorTable'.
getBGDT :: String -> IO BlockGroupDescriptorTable
getBGDT fn = do
  input <- BL.readFile fn
  return $ runGetL (skip 2048 >> readBlockGroupDescriptorTable) input

getInodeTableAtBlock :: BL.ByteString -> Int -> V.Vector Inode
getInodeTableAtBlock input blockNumber =
  let sb = runGetL (skip 1024 >> readSuperblock) input
  in flip runGetL input $ do
       skip (byteFromBlock sb blockNumber)
       readInodeTable (sb ^. inodesCount . to fromIntegral)

-- | Parses all tables out.
getAllTables :: BL.ByteString -> (Superblock, BlockGroupDescriptorTable, V.Vector Inode)
getAllTables input =
  flip runGetL input $ do
    skip 1024 -- Boot record/data
    sb <- readSuperblock
    skip 788
    bgdt <- readBlockGroupDescriptorTable
    let inodes = getInodeTableAtBlock input (bgdt ^. inodeTable . to fromIntegral)
    return (sb, bgdt, inodes)

-- | Lists names of all files in the root directory.
listRootFiles :: String -> IO [BL.ByteString]
listRootFiles fn = do
  input <- BL.readFile fn
  return $ flip runGetL input $ do
    skip 136192 -- TODO: Unhardcode
    rootDir <- readDirectory
    traverseDirs input rootDir [] (fromIntegral $ rootDir ^. recLen)
  where
    traverseDirs :: MonadGet m => BL.ByteString -> Directory -> [BL.ByteString] -> Int -> m [BL.ByteString]
    traverseDirs input d prev skipBytes =
      if BL.null (d ^. name) && (not . null $ prev)
      then return (sort . nub $ prev)  -- TODO: Why do we have to nub here?
      else do
        let next = flip runGetL input $ do
              skip (136192 + skipBytes)
              readDirectory
        traverseDirs input next ((d ^. name) : prev) (skipBytes + fromIntegral (next ^. recLen))
