{-# LANGUAGE OverloadedStrings #-}

module ByteTrain
  ( TrainSummary(..)
  , runTrainByte
  ) where

import ByteEncoding
import ByteModel
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), eitherDecode, withObject)
import qualified Data.ByteString.Lazy.Char8 as BL8
import DataSet
import Engram
import Muon
import Task

data TrainSummary = TrainSummary
  { trainPositiveCount :: Int
  , trainNegativeCount :: Int
  , trainEngramCount :: Int
  , trainFeatureSize :: Int
  , trainHiddenSize :: Int
  , trainOptimizer :: String
  , trainLossTrend :: [Double]
  } deriving (Show, Eq)

data TrainingRow
  = FunctionTestTrainingRow String String Bool
  | OtherTrainingRow
  deriving (Show, Eq)

instance FromJSON TrainingRow where
  parseJSON =
    withObject "TrainingRow" $ \object -> do
      kind <- object .:? "kind" .!= ""
      case kind of
        "function_test" ->
          FunctionTestTrainingRow
            <$> object .: "function_name"
            <*> object .: "generated_body"
            <*> object .: "testPassed"
        _ ->
          return OtherTrainingRow

data ByteExample = ByteExample
  { exampleTask :: TaskSpec
  , exampleExpression :: String
  , exampleLabel :: Double
  , exampleKind :: String
  } deriving (Show, Eq)

data BatchGradient = BatchGradient
  { gradientW1 :: [[Double]]
  , gradientB1 :: [Double]
  , gradientW2 :: [Double]
  , gradientB2 :: Double
  , gradientLoss :: Double
  } deriving (Show, Eq)

runTrainByte :: IO TrainSummary
runTrainByte = do
  rows <- readTrainingRows trainingLogPath
  let examples = buildExamples rows
  let engrams = buildEngramMemory rows
  let model0 = initialByteModel defaultFeatureSize defaultHiddenSize
  let momentum0 = zeroMatrixLike (modelW1 model0)
  let (trainedModel, lossTrend) = trainEpochs trainingEpochs examples model0 momentum0 []
  saveByteModel byteModelPath trainedModel
  saveEngrams engramMemoryPath engrams
  let summary =
        TrainSummary
          { trainPositiveCount = countExamples 1.0 examples
          , trainNegativeCount = countExamples 0.0 examples
          , trainEngramCount = length engrams
          , trainFeatureSize = modelFeatureSize trainedModel
          , trainHiddenSize = modelHiddenSize trainedModel
          , trainOptimizer = muonOptimizerName
          , trainLossTrend = summarizedLossTrend lossTrend
          }
  printSummary summary
  return summary

trainingEpochs :: Int
trainingEpochs =
  80

w1LearningRate :: Double
w1LearningRate =
  0.025

vectorLearningRate :: Double
vectorLearningRate =
  0.08

momentumBeta :: Double
momentumBeta =
  0.9

weightDecay :: Double
weightDecay =
  0.0001

readTrainingRows :: FilePath -> IO [TrainingRow]
readTrainingRows path = do
  content <- readFile path
  return (map decodeTrainingRow (lines content))

decodeTrainingRow :: String -> TrainingRow
decodeTrainingRow line =
  case eitherDecode (BL8.pack line) of
    Left _ ->
      OtherTrainingRow
    Right row ->
      row

buildExamples :: [TrainingRow] -> [ByteExample]
buildExamples rows =
  ensureCorePositives (rowExamples rows ++ explicitNegativeExamples)

rowExamples :: [TrainingRow] -> [ByteExample]
rowExamples rows =
  [ ByteExample task expression label "function_test"
  | FunctionTestTrainingRow functionName expression passed <- rows
  , Just task <- [taskForFunction functionName]
  , not (null expression)
  , let label = if passed then 1.0 else 0.0
  ]

explicitNegativeExamples :: [ByteExample]
explicitNegativeExamples =
  map (negativeExample sumTask) generatedSumWrongExpressions
    ++ map (negativeExample lengthTask) generatedLengthWrongExpressions

negativeExample :: TaskSpec -> String -> ByteExample
negativeExample task expression =
  ByteExample
    { exampleTask = task
    , exampleExpression = expression
    , exampleLabel = 0.0
    , exampleKind = "explicit_wrong_task_expression"
    }

ensureCorePositives :: [ByteExample] -> [ByteExample]
ensureCorePositives examples =
  ensurePositive lengthTask "length nums" (ensurePositive sumTask "sum nums" examples)

ensurePositive :: TaskSpec -> String -> [ByteExample] -> [ByteExample]
ensurePositive task expression examples =
  if any isMatchingPositive examples
    then examples
    else
      ByteExample
        { exampleTask = task
        , exampleExpression = expression
        , exampleLabel = 1.0
        , exampleKind = "core_positive_seed"
        }
        : examples
  where
    isMatchingPositive :: ByteExample -> Bool
    isMatchingPositive example =
      taskName (exampleTask example) == taskName task
        && exampleExpression example == expression
        && exampleLabel example >= 0.5

buildEngramMemory :: [TrainingRow] -> [Engram]
buildEngramMemory rows =
  mergeEngrams (rowEngrams rows ++ explicitNegativeEngrams)

rowEngrams :: [TrainingRow] -> [Engram]
rowEngrams rows =
  concatMap engramsForRow rows

engramsForRow :: TrainingRow -> [Engram]
engramsForRow row =
  case row of
    FunctionTestTrainingRow functionName expression passed ->
      case taskForFunction functionName of
        Nothing ->
          []
        Just task ->
          if passed
            then
              positiveEngram (taskName task) expression "passing-function-body"
                : memoryPatternEngrams task expression
            else [negativeEngram (taskName task) expression "failed-function-body"]
    OtherTrainingRow ->
      []

memoryPatternEngrams :: TaskSpec -> String -> [Engram]
memoryPatternEngrams task expression =
  if expression == "sum nums" || expression == "length nums"
    then [positiveEngram (taskName task) expression "chosen-memory-pattern"]
    else []

explicitNegativeEngrams :: [Engram]
explicitNegativeEngrams =
  map (negativeEngram (taskName sumTask) `withKind` "explicit-wrong-task") generatedSumWrongExpressions
    ++ map (negativeEngram (taskName lengthTask) `withKind` "explicit-wrong-task") generatedLengthWrongExpressions

withKind :: (String -> String -> Engram) -> String -> String -> Engram
withKind makeEngram kind expression =
  makeEngram expression kind

generatedSumWrongExpressions :: [String]
generatedSumWrongExpressions =
  [ "length nums"
  , "0"
  , "1"
  , "length [1, 2, 3]"
  , "length [1,2,3]"
  ]

generatedLengthWrongExpressions :: [String]
generatedLengthWrongExpressions =
  [ "sum nums"
  , "0"
  , "1"
  , "sum [1, 2, 3]"
  , "sum [1,2,3]"
  ]

taskForFunction :: String -> Maybe TaskSpec
taskForFunction functionName =
  case functionName of
    "generatedSum" ->
      Just sumTask
    "generatedLength" ->
      Just lengthTask
    _ ->
      Nothing

trainEpochs :: Int -> [ByteExample] -> ByteModel -> [[Double]] -> [Double] -> (ByteModel, [Double])
trainEpochs epochs examples model momentum losses
  | epochs <= 0 =
      (model, reverse losses)
  | null examples =
      (model, reverse losses)
  | otherwise =
      trainEpochs (epochs - 1) examples updatedModel updatedMomentum (epochLoss : losses)
  where
    gradient :: BatchGradient
    gradient =
      averageGradient model examples

    epochLoss :: Double
    epochLoss =
      gradientLoss gradient

    (updatedW1, updatedMomentum) =
      muonLiteUpdate w1LearningRate momentumBeta momentum (gradientW1 gradient) (modelW1 model)

    updatedModel :: ByteModel
    updatedModel =
      model
        { modelW1 = updatedW1
        , modelB1 = sgdVector vectorLearningRate (modelB1 model) (gradientB1 gradient)
        , modelW2 = sgdVectorWithDecay vectorLearningRate weightDecay (modelW2 model) (gradientW2 gradient)
        , modelB2 = modelB2 model - vectorLearningRate * gradientB2 gradient
        }

averageGradient :: ByteModel -> [ByteExample] -> BatchGradient
averageGradient model examples =
  scaleGradient (1.0 / fromIntegral (length examples)) summedGradient
  where
    summedGradient :: BatchGradient
    summedGradient =
      foldl addGradient (zeroGradient model) (map (exampleGradient model) examples)

exampleGradient :: ByteModel -> ByteExample -> BatchGradient
exampleGradient model example =
  BatchGradient
    { gradientW1 = w1Grad
    , gradientB1 = dz
    , gradientW2 = zipWith (*) (repeat dLogit) hidden
    , gradientB2 = dLogit
    , gradientLoss = logisticLoss label probability
    }
  where
    text :: String
    text =
      exampleFeatureText example

    features :: [(Int, Double)]
    features =
      hashedByteFeatures defaultMinNgram defaultMaxNgram (modelFeatureSize model) text

    (hidden, _, probability) =
      forwardByteFeatures model features

    label :: Double
    label =
      exampleLabel example

    dLogit :: Double
    dLogit =
      probability - label

    dz :: [Double]
    dz =
      [ dLogit * outputWeight * (1.0 - hiddenValue * hiddenValue)
      | (outputWeight, hiddenValue) <- zip (modelW2 model) hidden
      ]

    w1Grad :: [[Double]]
    w1Grad =
      [ denseFeatureGradient (modelFeatureSize model) features hiddenGrad
      | hiddenGrad <- dz
      ]

exampleFeatureText :: ByteExample -> String
exampleFeatureText example =
  candidateFeatureTextForExpression
    (Just (exampleTask example))
    (taskEnv (exampleTask example))
    ("function_body/" ++ exampleKind example)
    (exampleExpression example)

denseFeatureGradient :: Int -> [(Int, Double)] -> Double -> [Double]
denseFeatureGradient featureSize features scaleValue =
  go 0 sortedFeatures
  where
    sortedFeatures :: [(Int, Double)]
    sortedFeatures =
      features

    go :: Int -> [(Int, Double)] -> [Double]
    go index activeFeatures
      | index >= featureSize =
          []
      | otherwise =
          case activeFeatures of
            [] ->
              0.0 : go (index + 1) []
            (featureIndex, featureValue) : rest
              | index < featureIndex ->
                  0.0 : go (index + 1) activeFeatures
              | index == featureIndex ->
                  scaleValue * featureValue : go (index + 1) rest
              | otherwise ->
                  go index rest

logisticLoss :: Double -> Double -> Double
logisticLoss label probability =
  negate (label * log safeProbability + (1.0 - label) * log (1.0 - safeProbability))
  where
    safeProbability :: Double
    safeProbability =
      min 0.999999 (max 0.000001 probability)

zeroGradient :: ByteModel -> BatchGradient
zeroGradient model =
  BatchGradient
    { gradientW1 = zeroMatrixLike (modelW1 model)
    , gradientB1 = replicate (modelHiddenSize model) 0.0
    , gradientW2 = replicate (modelHiddenSize model) 0.0
    , gradientB2 = 0.0
    , gradientLoss = 0.0
    }

addGradient :: BatchGradient -> BatchGradient -> BatchGradient
addGradient left right =
  BatchGradient
    { gradientW1 = zipWithMatrix (+) (gradientW1 left) (gradientW1 right)
    , gradientB1 = zipWith (+) (gradientB1 left) (gradientB1 right)
    , gradientW2 = zipWith (+) (gradientW2 left) (gradientW2 right)
    , gradientB2 = gradientB2 left + gradientB2 right
    , gradientLoss = gradientLoss left + gradientLoss right
    }

scaleGradient :: Double -> BatchGradient -> BatchGradient
scaleGradient value gradient =
  BatchGradient
    { gradientW1 = scaleMatrix value (gradientW1 gradient)
    , gradientB1 = map (* value) (gradientB1 gradient)
    , gradientW2 = map (* value) (gradientW2 gradient)
    , gradientB2 = value * gradientB2 gradient
    , gradientLoss = value * gradientLoss gradient
    }

sgdVector :: Double -> [Double] -> [Double] -> [Double]
sgdVector learningRate weights gradient =
  zipWith (\weight grad -> weight - learningRate * grad) weights gradient

sgdVectorWithDecay :: Double -> Double -> [Double] -> [Double] -> [Double]
sgdVectorWithDecay learningRate decay weights gradient =
  zipWith (\weight grad -> weight - learningRate * (grad + decay * weight)) weights gradient

zipWithMatrix :: (Double -> Double -> Double) -> [[Double]] -> [[Double]] -> [[Double]]
zipWithMatrix combine =
  zipWith (zipWith combine)

scaleMatrix :: Double -> [[Double]] -> [[Double]]
scaleMatrix value =
  map (map (* value))

countExamples :: Double -> [ByteExample] -> Int
countExamples wantedLabel examples =
  length [ example | example <- examples, exampleLabel example == wantedLabel ]

summarizedLossTrend :: [Double] -> [Double]
summarizedLossTrend losses =
  case losses of
    [] ->
      []
    _ ->
      [ head losses
      , lossAtQuarter 1
      , lossAtQuarter 2
      , lossAtQuarter 3
      , last losses
      ]
  where
    lossAtQuarter :: Int -> Double
    lossAtQuarter quarter =
      losses !! min (length losses - 1) ((length losses * quarter) `div` 4)

printSummary :: TrainSummary -> IO ()
printSummary summary = do
  putStrLn ("positives: " ++ show (trainPositiveCount summary))
  putStrLn ("negatives: " ++ show (trainNegativeCount summary))
  putStrLn ("engrams: " ++ show (trainEngramCount summary))
  putStrLn ("feature size: " ++ show (trainFeatureSize summary))
  putStrLn ("hidden size: " ++ show (trainHiddenSize summary))
  putStrLn ("optimizer: " ++ trainOptimizer summary)
  putStrLn ("loss trend: " ++ show (trainLossTrend summary))
  putStrLn ("wrote model: " ++ byteModelPath)
  putStrLn ("wrote engrams: " ++ engramMemoryPath)
