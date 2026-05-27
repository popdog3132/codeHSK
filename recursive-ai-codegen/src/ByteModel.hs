{-# LANGUAGE DeriveGeneric #-}

module ByteModel
  ( ByteModel(..)
  , byteModelPath
  , defaultHiddenSize
  , initialByteModel
  , saveByteModel
  , loadByteModel
  , loadByteModelIfPresent
  , candidateFeatureText
  , candidateFeatureTextForExpression
  , scoreByteModel
  , scoreEngrams
  , scoreByteEngram
  , forwardByteFeatures
  , sigmoid
  ) where

import AST
import ByteEncoding
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, nub, sort)
import DataSet
import Engram
import GHC.Generics (Generic)
import Pretty
import System.Directory (doesFileExist)
import Task

data ByteModel = ByteModel
  { modelFeatureSize :: Int
  , modelHiddenSize :: Int
  , modelW1 :: [[Double]]
  , modelB1 :: [Double]
  , modelW2 :: [Double]
  , modelB2 :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON ByteModel
instance FromJSON ByteModel

byteModelPath :: FilePath
byteModelPath =
  "data/byte-model.json"

defaultHiddenSize :: Int
defaultHiddenSize =
  16

initialByteModel :: Int -> Int -> ByteModel
initialByteModel featureSize hiddenSize =
  ByteModel
    { modelFeatureSize = featureSize
    , modelHiddenSize = hiddenSize
    , modelW1 = initialW1
    , modelB1 = replicate hiddenSize 0.0
    , modelW2 = initialW2
    , modelB2 = 0.0
    }
  where
    initialW1 :: [[Double]]
    initialW1 =
      [ [ initialWeight hiddenIndex featureIndex
        | featureIndex <- [0 .. featureSize - 1]
        ]
      | hiddenIndex <- [0 .. hiddenSize - 1]
      ]

    initialW2 :: [Double]
    initialW2 =
      [ initialWeight hiddenIndex (featureSize + 17)
      | hiddenIndex <- [0 .. hiddenSize - 1]
      ]

initialWeight :: Int -> Int -> Double
initialWeight left right =
  fromIntegral (hashNgram 2001 [left + 13, right + 29] - 1000) / 40000.0

saveByteModel :: FilePath -> ByteModel -> IO ()
saveByteModel path model =
  BL.writeFile path (encode model)

loadByteModel :: FilePath -> IO (Either String ByteModel)
loadByteModel path = do
  content <- BL.readFile path
  return (eitherDecode content)

loadByteModelIfPresent :: FilePath -> IO (Maybe ByteModel)
loadByteModelIfPresent path = do
  exists <- doesFileExist path
  if exists
    then do
      decoded <- loadByteModel path
      case decoded of
        Left _ ->
          return Nothing
        Right model ->
          return (Just model)
    else return Nothing

candidateFeatureText :: Maybe TaskSpec -> Env -> String -> Expr -> String
candidateFeatureText maybeTask env scoringContext expr =
  candidateFeatureTextForExpression maybeTask env scoringContext (prettyExpr expr)

candidateFeatureTextForExpression :: Maybe TaskSpec -> Env -> String -> String -> String
candidateFeatureTextForExpression maybeTask env scoringContext expression =
  taskLines
    ++ "environment:\n"
    ++ concatMap envLine env
    ++ "scoring_context: "
    ++ scoringContext
    ++ "\n"
    ++ "candidate_expr: "
    ++ expression
    ++ "\n"
  where
    taskLines :: String
    taskLines =
      case maybeTask of
        Nothing ->
          "task_name: \ntask_description: \n"
        Just task ->
          "task_name: "
            ++ taskName task
            ++ "\n"
            ++ "task_description: "
            ++ taskDescription task
            ++ "\n"

    envLine :: (String, Type) -> String
    envLine (name, typeValue) =
      name ++ " :: " ++ prettyType typeValue ++ "\n"

scoreByteModel :: ByteModel -> String -> Double
scoreByteModel model text =
  clamp 0.0 100.0 (probability * 100.0)
  where
    features :: [(Int, Double)]
    features =
      hashedByteFeatures defaultMinNgram defaultMaxNgram (modelFeatureSize model) text

    (_, _, probability) =
      forwardByteFeatures model features

scoreEngrams :: [Engram] -> String -> Double
scoreEngrams engrams text =
  clamp (-100.0) 100.0 (positiveScore - negativePenalty)
  where
    taskNameValue :: String
    taskNameValue =
      fieldValue "task_name" text

    candidateExpression :: String
    candidateExpression =
      fieldValue "candidate_expr" text

    matchingEngrams :: [Engram]
    matchingEngrams =
      [ engram
      | engram <- engrams
      , taskMatches taskNameValue engram
      ]

    positiveScore :: Double
    positiveScore =
      maximumOrZero (map (positiveEngramScore candidateExpression) matchingEngrams)

    negativePenalty :: Double
    negativePenalty =
      maximumOrZero (map (negativeEngramPenalty candidateExpression) matchingEngrams)

scoreByteEngram :: ByteModel -> [Engram] -> String -> Double
scoreByteEngram model engrams text
  | engramScore >= 95.0 =
      100.0
  | otherwise =
      clamp 0.0 100.0 (0.75 * byteScore + 0.25 * engramScore)
  where
    byteScore :: Double
    byteScore =
      scoreByteModel model text

    engramScore :: Double
    engramScore =
      scoreEngrams engrams text

forwardByteFeatures :: ByteModel -> [(Int, Double)] -> ([Double], Double, Double)
forwardByteFeatures model features =
  (hiddenValues, rawScore, sigmoid rawScore)
  where
    hiddenPreActivations :: [Double]
    hiddenPreActivations =
      zipWith (+) (modelB1 model) [ dotSparse sortedFeatures row | row <- modelW1 model ]

    hiddenValues :: [Double]
    hiddenValues =
      map tanh hiddenPreActivations

    rawScore :: Double
    rawScore =
      modelB2 model + sum (zipWith (*) hiddenValues (modelW2 model))

    sortedFeatures :: [(Int, Double)]
    sortedFeatures =
      sortFeatures features

sigmoid :: Double -> Double
sigmoid value
  | value >= 0.0 =
      1.0 / (1.0 + exp (-value))
  | otherwise =
      expValue / (1.0 + expValue)
  where
    expValue :: Double
    expValue =
      exp value

dotSparse :: [(Int, Double)] -> [Double] -> Double
dotSparse features row =
  go 0 row features
  where
    go :: Int -> [Double] -> [(Int, Double)] -> Double
    go _ [] _ =
      0.0
    go _ _ [] =
      0.0
    go index (weight : restWeights) activeFeatures@((featureIndex, featureValue) : restFeatures)
      | index < featureIndex =
          go (index + 1) restWeights activeFeatures
      | index == featureIndex =
          featureValue * weight + go (index + 1) restWeights restFeatures
      | otherwise =
          go index (weight : restWeights) restFeatures

sortFeatures :: [(Int, Double)] -> [(Int, Double)]
sortFeatures features =
  sort features

fieldValue :: String -> String -> String
fieldValue fieldName text =
  case matchingLines of
    [] ->
      ""
    firstMatch : _ ->
      drop (length prefix) firstMatch
  where
    prefix :: String
    prefix =
      fieldName ++ ": "

    matchingLines :: [String]
    matchingLines =
      filter (isPrefixOf prefix) (lines text)

taskMatches :: String -> Engram -> Bool
taskMatches taskNameValue engram =
  null (engramTaskName engram)
    || null taskNameValue
    || engramTaskName engram == taskNameValue

positiveEngramScore :: String -> Engram -> Double
positiveEngramScore candidateExpression engram
  | engramSuccesses engram <= 0 =
      0.0
  | candidateExpression == engramExpression engram =
      100.0
  | otherwise =
      45.0 * expressionSimilarity candidateExpression (engramExpression engram)

negativeEngramPenalty :: String -> Engram -> Double
negativeEngramPenalty candidateExpression engram
  | engramFailures engram <= 0 =
      0.0
  | candidateExpression == engramExpression engram =
      95.0
  | otherwise =
      30.0 * expressionSimilarity candidateExpression (engramExpression engram)

expressionSimilarity :: String -> String -> Double
expressionSimilarity left right =
  if null unionTokens
    then 0.0
    else fromIntegral (length sharedTokens) / fromIntegral (length unionTokens)
  where
    leftTokens :: [String]
    leftTokens =
      nub (words left)

    rightTokens :: [String]
    rightTokens =
      nub (words right)

    sharedTokens :: [String]
    sharedTokens =
      [ token | token <- leftTokens, token `elem` rightTokens ]

    unionTokens :: [String]
    unionTokens =
      nub (leftTokens ++ rightTokens)

maximumOrZero :: [Double] -> Double
maximumOrZero values =
  case values of
    [] ->
      0.0
    _ ->
      maximum values

clamp :: Double -> Double -> Double -> Double
clamp low high value =
  min high (max low value)
