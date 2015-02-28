-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2.FeatureFlags
-- Copyright   : (C) 2015 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens
--
-- This module contains types for working with feature flags as found in
-- 'Superblock's.
----------------------------------------------------------------------------
module System.Ext2.FeatureFlags where

import Control.Lens
import Data.Bits ((.&.))
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import System.Ext2.Tables
import System.Ext2.Lens

data OptionalFeatureFlag = DirPrealloc
                         | AFSInodes
                         | Journaled
                         | ExtendedAttributes
                         | Resizable
                         | DirHashIndex
                         deriving (Eq, Show)

-- | Optional feature flags
optionalFeatureFlagTable :: [(OptionalFeatureFlag, Int)]
optionalFeatureFlagTable = [ (DirPrealloc, 0x0001)
                           , (AFSInodes, 0x0002)
                           , (Journaled, 0x0004)
                           , (ExtendedAttributes, 0x0008)
                           , (Resizable, 0x0010)
                           , (DirHashIndex, 0x0020)
                           ]

instance Enum OptionalFeatureFlag where
  fromEnum = fromJust . flip lookup optionalFeatureFlagTable
  toEnum = fromJust . flip lookup (map swap optionalFeatureFlagTable)

hasOptionalFlag :: Superblock -> OptionalFeatureFlag -> Bool
hasOptionalFlag sb o =
  sb ^. featureCompat .&. (fromIntegral . fromEnum $ o) == 1

-- | Read-only feature flags
data ROFeatureFlag = SparseSuperblocks
                   | LargeFileSize
                   | BTreeDirs
                   deriving (Eq, Show)

roCompatFlagTable :: [(ROFeatureFlag, Int)]
roCompatFlagTable = [ (SparseSuperblocks, 0x0001)
                    , (LargeFileSize, 0x0002)
                    , (BTreeDirs, 0x0004)
                    ]

instance Enum ROFeatureFlag where
  fromEnum = fromJust . flip lookup roCompatFlagTable
  toEnum = fromJust . flip lookup (map swap roCompatFlagTable)

hasRoFlag :: Superblock -> ROFeatureFlag -> Bool
hasRoFlag sb o =
  sb ^. featureRoCompat .&. (fromIntegral . fromEnum $ o) == 1

-- | Required feature flags
data RequiredFeatureFlag = Compression
                         | FileType
                         | ReplayJournal
                         | JournalDevice
                         | MetaBg -- TODO: What is this?
                         deriving (Eq, Show)

reqFlagTable :: [(RequiredFeatureFlag, Int)]
reqFlagTable = [ (Compression, 0x0001)
               , (FileType, 0x0002)
               , (ReplayJournal, 0x0004)
               , (JournalDevice, 0x0008)
               , (MetaBg, 0x0010)
               ]

instance Enum RequiredFeatureFlag where
  fromEnum = fromJust . flip lookup reqFlagTable
  toEnum = fromJust . flip lookup (map swap reqFlagTable)

hasReqFlag :: Superblock -> RequiredFeatureFlag -> Bool
hasReqFlag sb o =
  sb ^. featureIncompat .&. (fromIntegral . fromEnum $ o) == 1
