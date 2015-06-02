module Data.BTable.Put (
  put
  ) where

import           Data.Binary.Put
import           Data.Binary.IEEE754
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

numValues :: [Double] -> Int
numValues = length . filter (/= 0.0)

putLabels :: [String] -> Put
putLabels labels = do
  let sepStr = [(chr . fromIntegral) sep]
      joinedLabels = (intercalate sepStr labels)
      labelCount = fromIntegral (length joinedLabels)
  _ <- putWord32be labelCount
  _ <- putByteString . encodeUtf16BE . T.pack $ joinedLabels
  return ()

-- putRowValues :: [Double] -> Put
-- putRowValues row = mapM_ go (zip row [0..])
--   where go (v,idx)
--           | v == 0.0 = return
--           | otherwise = do
--               _ <- putWord32be idx
--               _ <- putFloat64be v
--               return
--
-- putRow row = do
--   _ <- putWord32be . fromIntegral . numValues $ row
--   _ <- putRowValues row
--   return
--
-- putRows :: [[Double]] -> Put
-- putRows rows = mapM_ putRow rows

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
