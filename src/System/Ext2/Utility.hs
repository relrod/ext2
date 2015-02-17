-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2.Utility
-- Copyright   : (C) 2015 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens
--
-- This module exports utility functions for working with ext2 after it has been
-- parsed into Haskell types.
----------------------------------------------------------------------------
module System.Ext2.Utility (
  -- * Utility functions
    blockGroupCount
  , byteFromBlock

  -- * Silly test/helper functions
  , getSuperblock
  , getBGDT
  , getAllTables
  , listRootFiles

  ) where

import Control.Lens
import Data.Bits
import Data.Bytes.Get
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (nub, sort)
import qualified Data.Vector as V
import System.Ext2
import System.Ext2.Lens

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
