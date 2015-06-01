{-# LANGUAGE OverloadedStrings #-}

module Data.BTable where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Binary
import Data.Binary.Get
import qualified Data.Map as Map
import Data.String.Unicode

-- ASCII separator character
sep :: Word8
sep = 31

word32ToInt :: Word32 -> Int
word32ToInt w = (fromIntegral w) :: Int

-- intToWord32 :: Int -> Word32
-- intToWord32 i = (fromIntegral i) :: Word32
--
-- materialize :: Int -> [(Word32, Word64)] -> [Double]
-- materialize fc indices =
--     let indexMap = Map.fromList indices
--         lookupIdx k = case Map.lookup (intToWord32 k) indexMap of
--             Just v  -> (fromIntegral v) :: Double
--             Nothing -> 0.0 :: Double
--     in map lookupIdx [0..fc]
--
-- -- TODO: can we get a `Get [32, 64]` signature here?
-- readIndices :: Int -> [Get (Word32, Word64)]
-- readIndices fc = map readPair [0..fc]
--     where readPair _ = do
--               idx <- getWord32be
--               v <- getWord64be
--               return (idx, v)
--
-- getRow :: Int -> Get [Double] -- TODO: might be incorrect
-- getRow fc = do
--   indices <- readIndices fc -- TODO: have `[Get (32, 64)]`, need `Get [32, 64]`
--   return $ materialize fc indices
--
-- getRows :: Int -> Get [[Double]]
-- getRows fc = do
--     empty <- isEmpty
--     if empty
--         then return [[]]
--         else do
--           row <- getRow fc
--           rows <- getRows fc
--           return (row:rows)

getRows fc = undefined

readVersion :: Get Word32
readVersion = do
    version <- getWord32be
    return version

readLabels :: Get [String]
readLabels = do
    version <- readVersion
    labelCount <- getWord32be
    rawLabels <- getByteString $ 2 * (word32ToInt labelCount)
    return $ map (utf16beToUnicode . BC.unpack) (B.split sep rawLabels)

readRows :: Get [[Double]]
readRows = do
    version <- readVersion
    labels <- readLabels
    let featureCount = length labels
    return $ getRows featureCount
