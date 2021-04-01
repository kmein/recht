module Recht.Cache (cached) where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B (hGet, length)
import Data.Time
import Recht.Util (blockSize)
import System.Directory
import System.FilePath
import System.IO

-- Taken from https://hackage.haskell.org/package/binary-0.8.8.0/docs/src/Data.Binary.html#decodeFileOrFail
-- to adjust chunk size to 8M instead of 32K (the Haskell default).
decodeFileOrFail' :: Binary a => FilePath -> IO (Either (ByteOffset, String) a)
decodeFileOrFail' f =
  withBinaryFile f ReadMode $ \h -> do
    feed (runGetIncremental get) h
  where
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos string) _ = return (Left (pos, string))
    feed (Partial k) h = do
      chunk <- B.hGet h blockSize
      case B.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h

cached :: (Binary a) => FilePath -> IO a -> IO a
cached cacheName ioAction = do
  cacheDirectoryPath <- getXdgDirectory XdgCache "recht"
  createDirectoryIfMissing True cacheDirectoryPath
  let cacheFile = cacheDirectoryPath </> map escape cacheName
  cachedFileExists <- doesFileExist cacheFile
  if cachedFileExists
    then do
      cacheTooOld <- isCacheInvalidated cacheFile
      if cacheTooOld
        then runAndCache cacheFile
        else do
          cachedFileContents <- decodeFileOrFail' cacheFile
          case cachedFileContents of
            Right decoded -> pure decoded
            _ -> runAndCache cacheFile
    else runAndCache cacheFile
  where
    isCacheInvalidated file = do
      modificationTime <- getModificationTime file
      now <- getCurrentTime
      let cacheAge = diffUTCTime now modificationTime
      return $ cacheAge >= nominalDay
    escape '/' = '_'
    escape x = x
    runAndCache file = do
      result <- ioAction
      result <$ encodeFile file result
