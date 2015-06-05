module Data.BTable.Put (
  put
  ) where

import           Data.Binary.Put
import           Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as BL
import           Data.Char (chr)
import           Data.Foldable (traverse_)
import           Data.List (intercalate)
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
      joinedLabels = (intercalate sepStr labels)
      labelCount = fromIntegral (length joinedLabels)
  putWord32be labelCount
  putByteString . encodeUtf16BE . T.pack $ joinedLabels
  return ()

putRow :: [Double] -> Put
putRow row = do
  putWord32be $ fromIntegral (numValues row)
  traverse_ putPair $ zip row ([0..] :: [Integer])
  where numValues = length . filter (/= 0.0)
        putPair (v,idx)
          | v == 0.0 = return ()
          | otherwise = do
              putWord32be (fromIntegral idx)
              putFloat64be v
              return ()

put' :: [String] -> [[Double]] -> PutM [()]
put' labels rows = do
  putVersion
  putLabels labels
  mapM putRow rows

put :: FilePath -> [String] -> [[Double]] -> IO ()
put path labels rows =
  BL.writeFile path $ snd (runPutM (put' labels rows))
