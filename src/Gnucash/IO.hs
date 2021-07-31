module Gnucash.IO
  ( readGnucashDocument,
    readXmlGnucashDocument,
    writeXmlGnucashDocument,
  )
where

import Codec.Compression.GZip (decompress)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import GHC.IO.Handle (hClose)
import Gnucash.Builder (buildDocument)
import Gnucash.Parser (parseBook)
import Gnucash.Types (Book)
import Magic
  ( MagicFlag (MagicMimeType),
    magicFile,
    magicLoadDefault,
    magicOpen,
  )
import System.IO (Handle)
import System.IO.Temp (withSystemTempFile)
import Text.XML.HXT.Core

-- | Read a (possibly compressed) gnucash file and return all books declared in it.
readGnucashDocument :: (MonadIO m, MonadMask m) => FilePath -> m [Book]
readGnucashDocument inputPath = do
  needsDecompression <- liftIO $ isGzipped inputPath
  if needsDecompression
    then withSystemTempFile "gnucash2hledger.xml" $
      \tmpPath tmpHandle -> do
        liftIO $
          decompressToTempFile inputPath tmpHandle
            `finally` hClose tmpHandle
        readXmlGnucashDocument tmpPath
    else readXmlGnucashDocument inputPath
{-# SCC readGnucashDocument #-}

-- | Read an uncompressed gnucash file and return all books declared in it.
readXmlGnucashDocument :: MonadIO m => FilePath -> m [Book]
readXmlGnucashDocument path =
  liftIO . runX $
    readDocument [withValidate no, withRemoveWS yes] path
      >>> parseBook

decompressToTempFile :: FilePath -> Handle -> IO ()
decompressToTempFile inputPath tmpHandle = do
  compressedContents <- BS.readFile inputPath
  let decompressedContents = decompress compressedContents
  BS.hPut tmpHandle decompressedContents

isGzipped :: FilePath -> IO Bool
isGzipped path = do
  magic <- magicOpen [MagicMimeType]
  magicLoadDefault magic
  mimeType <- magicFile magic path
  return $ mimeType == "application/gzip"

-- | Write a list of books to an uncompressed gnucash file.
writeXmlGnucashDocument :: (MonadIO m, MonadFail m) => FilePath -> [Book] -> m Int
writeXmlGnucashDocument path books = do
  [statusCode] <-
    liftIO . runX $
      buildDocument books
        >>> writeDocument [withOutputEncoding utf8, withIndent yes] path
        >>> getErrStatus
  pure statusCode
{-# SCC writeXmlGnucashDocument #-}
