module ByteEncoding
  ( bytesOfString
  , byteNgrams
  , byteNgramsRange
  , hashNgram
  , hashedByteFeatures
  , defaultMinNgram
  , defaultMaxNgram
  , defaultFeatureSize
  ) where

import qualified Data.ByteString as BS
import Data.Bits (xor)
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

defaultMinNgram :: Int
defaultMinNgram =
  1

defaultMaxNgram :: Int
defaultMaxNgram =
  8

defaultFeatureSize :: Int
defaultFeatureSize =
  8192

bytesOfString :: String -> [Int]
bytesOfString text =
  map fromIntegral (BS.unpack (TE.encodeUtf8 (T.pack text)))

byteNgrams :: Int -> [Int] -> [[Int]]
byteNgrams n bytes
  | n <= 0 =
      []
  | length bytes < n =
      []
  | otherwise =
      take n bytes : byteNgrams n (drop 1 bytes)

byteNgramsRange :: Int -> Int -> String -> [[Int]]
byteNgramsRange minN maxN text =
  concatMap (`byteNgrams` bytes) [minN .. maxN]
  where
    bytes :: [Int]
    bytes =
      bytesOfString text

hashNgram :: Int -> [Int] -> Int
hashNgram featureSize ngram
  | featureSize <= 0 =
      0
  | otherwise =
      foldl hashStep seed ngram `mod` featureSize
  where
    seed :: Int
    seed =
      2166136261 + length ngram * 16777619

    hashStep :: Int -> Int -> Int
    hashStep current byte =
      (current * 16777619) `xor` (byte + 31)

hashedByteFeatures :: Int -> Int -> Int -> String -> [(Int, Double)]
hashedByteFeatures minN maxN featureSize text
  | featureSize <= 0 =
      []
  | otherwise =
      normalizeCounts (group (sort hashed))
  where
    hashed :: [Int]
    hashed =
      map (hashNgram featureSize) (byteNgramsRange minN maxN text)

    total :: Double
    total =
      sqrt (fromIntegral (max 1 (length hashed)))

    normalizeCounts :: [[Int]] -> [(Int, Double)]
    normalizeCounts grouped =
      [ (featureIndex, fromIntegral (length matches) / total)
      | matches@(featureIndex : _) <- grouped
      ]
