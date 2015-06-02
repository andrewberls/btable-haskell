module Data.BTable.Put (
  put
  ) where

import           Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.List
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Word

-- ASCII separator character
sep :: Word8
sep = 31

putVersion :: Put
putVersion = putWord32be 0

putLabels :: [String] -> Put
putLabels labels = do
  let sepStr = [(chr . fromIntegral) sep]
      joinedLabels = encodeUtf16BE $ T.pack (intercalate sepStr labels)
  _ <- putWord32be $ fromIntegral (B.length joinedLabels)
  _ <- putByteString joinedLabels
  return ()

putRows :: [[Double]] -> Put
putRows rows = undefined

put' :: [String] -> [[Double]] -> Put
put' labels rows = do
  _ <- putVersion
  _ <- putLabels labels
  _ <- putRows rows
  return ()

put :: FilePath -> [String] -> [[Double]] -> IO ()
put path labels rows =
  BL.writeFile path $ runPut (put' labels rows)
