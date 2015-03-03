module Main where

import Control.Lens
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bytes.Get
import Data.List (intercalate)
import System.Ext2
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "Usage: ext2checker <path to ext2 filesystem>"
  fs <- BL.readFile (head args)
  let s = flip runGetL fs $ skip 1024 >> readSuperblock
  putStrLn $ "FS Size: " ++ show (fsSize s) ++ " Bytes"
  putStrLn $ "Unallocated: " ++ show (unallocated s) ++ " Bytes"
  putStrLn $ "FS State: " ++  s ^. state . to show
  putStrLn $ "Required feature flags: " ++
    (intercalate ", " . map show $ s ^. featureCompat)
  putStrLn $ "Optional feature flags: " ++
    (intercalate ", " . map show $ s ^. featureIncompat)
  putStrLn $ "Read-only feature flags: " ++
    (intercalate ", " . map show $ s ^. featureRoCompat)

  where
    fsSize :: Superblock -> Double
    fsSize s =
      fromIntegral
      ((s ^. blocksCount) *
       (1024 `shiftL` fromIntegral (s ^. logBlockSize)))

    unallocated :: Superblock -> Double
    unallocated s =
      fromIntegral
      ((s ^. freeBlocksCount) *
       (1024 `shiftL` fromIntegral (s ^. logBlockSize)))
