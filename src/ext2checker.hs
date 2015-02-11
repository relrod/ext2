module Main where

import Control.Monad
import Control.Monad.Trans
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
  let superblock = flip runGetL fs $ skip 1024 >> readSuperblock
  print superblock
