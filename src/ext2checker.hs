module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bytes.Get
import System.Ext2
import System.Ext2.FSChecks
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "Usage: ext2checker <path to ext2 filesystem>"
  fs <- BL.readFile (head args)
  let s = flip runGetL fs $ skip 1024 >> readSuperblock
  putStrLn $  "FS Size: " ++ show (fromIntegral ((s ^. blocksCount) * (1024 `shiftL` (fromIntegral (s ^. logBlockSize)))) / 1024 / 1024) ++ "MB"
