module Muon
  ( muonOptimizerName
  , zeroMatrixLike
  , muonLiteUpdate
  ) where

muonOptimizerName :: String
muonOptimizerName =
  "muon-lite-w1-newton-schulz+sgd-vectors"

zeroMatrixLike :: [[Double]] -> [[Double]]
zeroMatrixLike matrix =
  map (map (const 0.0)) matrix

muonLiteUpdate :: Double -> Double -> [[Double]] -> [[Double]] -> [[Double]] -> ([[Double]], [[Double]])
muonLiteUpdate learningRate beta previousMomentum gradient weights =
  (updatedWeights, nextMomentum)
  where
    nextMomentum :: [[Double]]
    nextMomentum =
      zipWithMatrix momentumStep previousMomentum gradient

    momentumStep :: Double -> Double -> Double
    momentumStep oldMomentum grad =
      beta * oldMomentum + (1.0 - beta) * grad

    orthogonalUpdate :: [[Double]]
    orthogonalUpdate =
      orthogonalizeRows nextMomentum

    updatedWeights :: [[Double]]
    updatedWeights =
      zipWithMatrix (\weight update -> weight - learningRate * update) weights orthogonalUpdate

orthogonalizeRows :: [[Double]] -> [[Double]]
orthogonalizeRows matrix
  | frobenius <= 1.0e-12 =
      matrix
  | otherwise =
      newtonSchulz 2 normalized
  where
    frobenius :: Double
    frobenius =
      sqrt (sum (map (sum . map square) matrix))

    normalized :: [[Double]]
    normalized =
      scaleMatrix (1.0 / frobenius) matrix

newtonSchulz :: Int -> [[Double]] -> [[Double]]
newtonSchulz steps matrix
  | steps <= 0 =
      matrix
  | otherwise =
      newtonSchulz (steps - 1) nextMatrix
  where
    gram :: [[Double]]
    gram =
      rowGram matrix

    gramTimesMatrix :: [[Double]]
    gramTimesMatrix =
      leftMultiply gram matrix

    nextMatrix :: [[Double]]
    nextMatrix =
      zipWithMatrix (\x gx -> 1.5 * x - 0.5 * gx) matrix gramTimesMatrix

rowGram :: [[Double]] -> [[Double]]
rowGram rows =
  [ [ dot left right | right <- rows ] | left <- rows ]

leftMultiply :: [[Double]] -> [[Double]] -> [[Double]]
leftMultiply left right =
  [ combineRows coeffs right | coeffs <- left ]

combineRows :: [Double] -> [[Double]] -> [Double]
combineRows coeffs rows =
  foldl (zipWith (+)) zeroRow scaledRows
  where
    zeroRow :: [Double]
    zeroRow =
      case rows of
        [] ->
          []
        firstRow : _ ->
          map (const 0.0) firstRow

    scaledRows :: [[Double]]
    scaledRows =
      zipWith scaleRow coeffs rows

scaleMatrix :: Double -> [[Double]] -> [[Double]]
scaleMatrix value =
  map (scaleRow value)

scaleRow :: Double -> [Double] -> [Double]
scaleRow value =
  map (* value)

zipWithMatrix :: (Double -> Double -> Double) -> [[Double]] -> [[Double]] -> [[Double]]
zipWithMatrix combine =
  zipWith (zipWith combine)

dot :: [Double] -> [Double] -> Double
dot left right =
  sum (zipWith (*) left right)

square :: Double -> Double
square value =
  value * value
