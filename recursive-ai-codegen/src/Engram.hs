{-# LANGUAGE DeriveGeneric #-}

module Engram
  ( Engram(..)
  , engramMemoryPath
  , saveEngrams
  , loadEngrams
  , loadEngramsIfPresent
  , positiveEngram
  , negativeEngram
  , mergeEngrams
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

data Engram = Engram
  { engramTaskName :: String
  , engramExpression :: String
  , engramKind :: String
  , engramSuccesses :: Int
  , engramFailures :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Engram
instance FromJSON Engram

engramMemoryPath :: FilePath
engramMemoryPath =
  "data/engram-memory.json"

saveEngrams :: FilePath -> [Engram] -> IO ()
saveEngrams path engrams =
  BL.writeFile path (encode engrams)

loadEngrams :: FilePath -> IO (Either String [Engram])
loadEngrams path = do
  content <- BL.readFile path
  return (eitherDecode content)

loadEngramsIfPresent :: FilePath -> IO (Maybe [Engram])
loadEngramsIfPresent path = do
  exists <- doesFileExist path
  if exists
    then do
      decoded <- loadEngrams path
      case decoded of
        Left _ ->
          return Nothing
        Right engrams ->
          return (Just engrams)
    else return Nothing

positiveEngram :: String -> String -> String -> Engram
positiveEngram taskName expression kind =
  Engram
    { engramTaskName = taskName
    , engramExpression = expression
    , engramKind = kind
    , engramSuccesses = 1
    , engramFailures = 0
    }

negativeEngram :: String -> String -> String -> Engram
negativeEngram taskName expression kind =
  Engram
    { engramTaskName = taskName
    , engramExpression = expression
    , engramKind = kind
    , engramSuccesses = 0
    , engramFailures = 1
    }

mergeEngrams :: [Engram] -> [Engram]
mergeEngrams engrams =
  foldl insertEngram [] engrams

insertEngram :: [Engram] -> Engram -> [Engram]
insertEngram existing newEngram =
  case existing of
    [] ->
      [newEngram]
    current : rest ->
      if sameEngram current newEngram
        then combineEngrams current newEngram : rest
        else current : insertEngram rest newEngram

sameEngram :: Engram -> Engram -> Bool
sameEngram left right =
  engramTaskName left == engramTaskName right
    && engramExpression left == engramExpression right
    && engramKind left == engramKind right

combineEngrams :: Engram -> Engram -> Engram
combineEngrams left right =
  left
    { engramSuccesses = engramSuccesses left + engramSuccesses right
    , engramFailures = engramFailures left + engramFailures right
    }
