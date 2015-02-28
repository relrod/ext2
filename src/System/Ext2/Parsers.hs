-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2.Parsers
-- Copyright   : (C) 2015 Ricky Elrod
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
module System.Ext2.Parsers (
    -- * Superblock
    Superblock (..)
    -- ** Parsers
  , readSuperblock

    -- * BlockGroupDescriptorTable
  , BlockGroupDescriptorTable (..)
    -- ** Parsers
  , readBlockGroupDescriptorTable

    -- * Inode
  , Inode (..)
    -- ** Parsers
  , readInode
  , readInodeTable

    -- * Directory
  , Directory (..)
    -- ** Parsers
  , readDirectory
  ) where

import Control.Applicative
import Data.Bytes.Get
import qualified Data.Vector as V
import System.Ext2.Tables

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
             <*> ((,,,)
                  <$> getWord32le
                  <*> getWord32le
                  <*> getWord32le
                  <*> getWord32le)
             <*> getWord8
             <*> getWord32le
             <*> getWord32le

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
        <*> ((,,,,,,,,,,,,,,)
             <$> getWord32le
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
             <*> getWord32le
             <*> getWord32le)
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getLazyByteString 12

readInodeTable :: MonadGet m => Int ->  m (V.Vector Inode)
readInodeTable n = V.replicateM n readInode

readDirectory :: MonadGet m => m Directory
readDirectory = do
  inode' <- getWord32le
  recLen' <- getWord16le
  nameLen' <- getWord8
  fileType' <- getWord8
  name' <- getLazyByteString (fromIntegral nameLen')
  padding' <- getLazyByteString 3
  return $ Directory inode' recLen' nameLen' fileType' name' padding'
