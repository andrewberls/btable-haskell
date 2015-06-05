module Data.BTable.Get (
  getVersion,
  getLabels,
  getRows,
  ) where

import           Data.Binary.Get
import           Data.Binary.IEEE754
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import           Data.String.Unicode
import           Data.Word

-- ASCII separator character
sep :: Word8
sep = 31

readInt :: Get Int
readInt = fromIntegral <$> getWord32be

readVersion :: Get Int
readVersion = readInt

readLabels' :: Get [String]
readLabels' = do
  labelCount <- readInt
  rawLabels <- getByteString $ 2 * labelCount
  return $ map (utf16beToUnicode . BC.unpack) (B.split sep rawLabels)

readLabels :: Get [String]
readLabels = do
  readVersion
  readLabels'

materialize :: Int -> [(Word32, Double)] -> [Double]
materialize fc indices =
  let indexMap = Map.fromList indices
      lookupIdx k = maybe 0.0 id $ Map.lookup (fromIntegral k) indexMap
  in map lookupIdx [0..(fc - 1)]

readIndices :: Int -> [Get (Word32, Double)]
readIndices nvals = map readPair [0..(nvals - 1)]
  where readPair _ = do
          idx <- getWord32be
          v <- getFloat64be
          return (idx, v)

readRow :: Int -> Get [Double]
readRow fc = do
  nvals <- readInt
  indices <- sequence $ readIndices nvals
  return $ materialize fc indices

readRows :: Get [[Double]]
readRows = do
  readVersion
  labels <- readLabels'
  let featureCount = length labels
  readRows' featureCount
  where readRows' fc = do
          row <- readRow fc
          empty <- isEmpty
          if empty
             then return [row]
             else do
               rows <- readRows' fc
               return (row:rows)

getVersion :: FilePath -> IO Int
getVersion path = do
  contents <- BL.readFile path
  return $ runGet readVersion contents

getLabels :: FilePath -> IO [String]
getLabels path = do
  contents <- BL.readFile path
  return $ runGet readLabels contents

getRows :: FilePath -> IO [[Double]]
getRows path = do
  contents <- BL.readFile path
  return $ runGet readRows contents
