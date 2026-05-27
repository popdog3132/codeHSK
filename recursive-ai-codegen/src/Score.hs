module Score
  ( ScoredExpr(..)
  , ScoreContext(..)
  , scoreExpr
  , scoreExprFake
  , scoreExprFakeWithContext
  , scoreExprFallback
  , scoreExprFallbackWithContext
  , scoreExprLLM
  , scoreExprLLMWithContext
  , scoreExprBatchLLMWithContext
  , scoreExprFakeForTask
  , scoreExprLLMForTask
  , scoreExprBatchLLMForTask
  , scoreContextName
  ) where

import AST
import Config
import Control.Concurrent (MVar, forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, throwIO)
import DataSet
import LLM
import Prompt
import ScoringContext
import System.IO (hPutStrLn, stderr)
import Task

data ScoredExpr = ScoredExpr
  { scoredExpr :: Expr
  , scoredScore :: Double
  , scoredReason :: String
  , scoredSource :: String
  , scoredRawLLMScore :: Maybe Double
  , scoredFakeScore :: Double
  , scoredBlended :: Bool
  , scoredContext :: String
  , scoredTaskName :: Maybe String
  , scoredByteModelScore :: Maybe Double
  , scoredEngramScore :: Maybe Double
  , scoredByteEngramScore :: Maybe Double
  , scoredFinalScore :: Double
  , scoredOptimizer :: Maybe String
  , scoredModelPath :: Maybe FilePath
  , scoredEngramPath :: Maybe FilePath
  } deriving (Show, Eq)

scoreExpr :: Type -> Expr -> ScoredExpr
scoreExpr =
  scoreExprFake

scoreExprFake :: Type -> Expr -> ScoredExpr
scoreExprFake =
  scoreExprFakeWithContext SearchScoring

scoreExprFakeWithContext :: ScoreContext -> Type -> Expr -> ScoredExpr
scoreExprFakeWithContext context goalType expr =
  ScoredExpr
    { scoredExpr = expr
    , scoredScore = totalScore
    , scoredReason = reason
    , scoredSource = "fake"
    , scoredRawLLMScore = Nothing
    , scoredFakeScore = totalScore
    , scoredBlended = False
    , scoredContext = scoreContextName context
    , scoredTaskName = Nothing
    , scoredByteModelScore = Nothing
    , scoredEngramScore = Nothing
    , scoredByteEngramScore = Nothing
    , scoredFinalScore = totalScore
    , scoredOptimizer = Nothing
    , scoredModelPath = Nothing
    , scoredEngramPath = Nothing
    }
  where
    matchesGoal :: Bool
    matchesGoal =
      exprType expr == goalType

    typeScore :: Double
    typeScore =
      if matchesGoal then 100.0 else -1000.0

    sizeBonus :: Double
    sizeBonus =
      20.0 / fromIntegral (exprSize expr)

    usefulBonus :: Double
    usefulBonus =
      usefulFormBonus expr

    incompleteBonus :: Double
    incompleteBonus =
      incompleteFormBonus context expr

    finalHolePenalty :: Double
    finalHolePenalty =
      if context == FunctionBodyScoring && exprHasHole expr then -250.0 else 0.0

    totalScore :: Double
    totalScore =
      typeScore + sizeBonus + usefulBonus + incompleteBonus + finalHolePenalty

    typeReason :: String
    typeReason =
      if matchesGoal then "type matches goal" else "wrong type"

    reason :: String
    reason =
      typeReason
        ++ ", size bonus " ++ show sizeBonus
        ++ ", useful form bonus " ++ show usefulBonus
        ++ ", incomplete expression bonus " ++ show incompleteBonus
        ++ ", final hole penalty " ++ show finalHolePenalty
        ++ ", scoring context " ++ scoreContextName context

scoreExprFallback :: Type -> Expr -> ScoredExpr
scoreExprFallback =
  scoreExprFallbackWithContext SearchScoring

scoreExprFallbackWithContext :: ScoreContext -> Type -> Expr -> ScoredExpr
scoreExprFallbackWithContext context goalType expr =
  fakeScore
    { scoredReason = scoredReason fakeScore ++ ", LLM unavailable"
    , scoredSource = "fallback"
    }
  where
    fakeScore :: ScoredExpr
    fakeScore =
      scoreExprFakeWithContext context goalType expr

scoreExprFakeForTask :: ScoreContext -> TaskSpec -> Expr -> ScoredExpr
scoreExprFakeForTask context task expr =
  taskScore
    { scoredScore = finalScore
    , scoredReason =
        scoredReason taskScore
          ++ ", task "
          ++ taskName task
          ++ ", task adjustment "
          ++ show adjustment
          ++ ", "
          ++ taskAdjustmentReason task expr
    , scoredFakeScore = finalScore
    , scoredFinalScore = finalScore
    , scoredTaskName = Just (taskName task)
    }
  where
    taskScore :: ScoredExpr
    taskScore =
      scoreExprFakeWithContext context (taskGoalType task) expr

    adjustment :: Double
    adjustment =
      taskAdjustment task expr

    finalScore :: Double
    finalScore =
      scoredScore taskScore + adjustment

usefulFormBonus :: Expr -> Double
usefulFormBonus expr =
  case expr of
    Sum _ ->
      8.0
    Length _ ->
      7.0
    If _ _ _ ->
      6.0
    Add _ _ ->
      5.0
    Eq _ _ ->
      4.0
    Mul _ _ ->
      3.0
    _ ->
      0.0

incompleteFormBonus :: ScoreContext -> Expr -> Double
incompleteFormBonus context expr =
  if context == HoleExpansionScoring && exprHasHole expr
    then
      case expr of
        Add _ _ ->
          16.0
        If _ _ _ ->
          12.0
        Mul _ _ ->
          10.0
        Eq _ _ ->
          8.0
        Sum _ ->
          6.0
        Length _ ->
          6.0
        _ ->
          0.0
    else 0.0

taskAdjustment :: TaskSpec -> Expr -> Double
taskAdjustment task expr =
  case taskName task of
    "generatedSum" ->
      sumTaskAdjustment expr
    "generatedLength" ->
      lengthTaskAdjustment expr
    _ ->
      0.0

taskAdjustmentReason :: TaskSpec -> Expr -> String
taskAdjustmentReason task expr =
  case taskName task of
    "generatedSum" ->
      sumTaskReason expr
    "generatedLength" ->
      lengthTaskReason expr
    _ ->
      "no task-specific heuristic"

sumTaskAdjustment :: Expr -> Double
sumTaskAdjustment expr
  | isSumNums expr = 120.0
  | isLengthNums expr = -110.0
  | isConstantInt expr = -120.0
  | containsHardcodedList expr = -130.0
  | isUnrelatedVariable expr = -80.0
  | isAnySum expr = 45.0
  | isAnyLength expr = -70.0
  | otherwise = 0.0

sumTaskReason :: Expr -> String
sumTaskReason expr
  | isSumNums expr = "sum task: direct sum nums"
  | isLengthNums expr = "sum task: length nums solves the wrong task"
  | isConstantInt expr = "sum task: constant Int ignores nums"
  | containsHardcodedList expr = "sum task: hardcoded list literal ignores input nums"
  | isUnrelatedVariable expr = "sum task: unrelated variable ignores nums"
  | isAnySum expr = "sum task: sum-shaped partial candidate"
  | isAnyLength expr = "sum task: length-shaped candidate solves the wrong task"
  | otherwise = "sum task: no direct task heuristic"

lengthTaskAdjustment :: Expr -> Double
lengthTaskAdjustment expr
  | isLengthNums expr = 120.0
  | isSumNums expr = -110.0
  | isConstantInt expr = -120.0
  | containsHardcodedList expr = -130.0
  | isUnrelatedVariable expr = -80.0
  | isAnyLength expr = 45.0
  | isAnySum expr = -70.0
  | otherwise = 0.0

lengthTaskReason :: Expr -> String
lengthTaskReason expr
  | isLengthNums expr = "length task: direct length nums"
  | isSumNums expr = "length task: sum nums solves the wrong task"
  | isConstantInt expr = "length task: constant Int ignores nums"
  | containsHardcodedList expr = "length task: hardcoded list literal ignores input nums"
  | isUnrelatedVariable expr = "length task: unrelated variable ignores nums"
  | isAnyLength expr = "length task: length-shaped partial candidate"
  | isAnySum expr = "length task: sum-shaped candidate solves the wrong task"
  | otherwise = "length task: no direct task heuristic"

isSumNums :: Expr -> Bool
isSumNums expr =
  case expr of
    Sum listExpr ->
      isNumsList listExpr
    _ ->
      False

isLengthNums :: Expr -> Bool
isLengthNums expr =
  case expr of
    Length listExpr ->
      isNumsList listExpr
    _ ->
      False

isNumsList :: Expr -> Bool
isNumsList expr =
  case expr of
    Var "nums" (TList TInt) ->
      True
    _ ->
      False

isConstantInt :: Expr -> Bool
isConstantInt expr =
  case expr of
    LitInt _ ->
      True
    _ ->
      False

isUnrelatedVariable :: Expr -> Bool
isUnrelatedVariable expr =
  case expr of
    Var name _ ->
      name /= "nums"
    _ ->
      False

isAnySum :: Expr -> Bool
isAnySum expr =
  case expr of
    Sum _ ->
      True
    _ ->
      False

isAnyLength :: Expr -> Bool
isAnyLength expr =
  case expr of
    Length _ ->
      True
    _ ->
      False

containsHardcodedList :: Expr -> Bool
containsHardcodedList expr =
  case expr of
    Var _ _ ->
      False
    Hole _ _ ->
      False
    LitInt _ ->
      False
    LitBool _ ->
      False
    Add left right ->
      containsHardcodedList left || containsHardcodedList right
    Mul left right ->
      containsHardcodedList left || containsHardcodedList right
    Eq left right ->
      containsHardcodedList left || containsHardcodedList right
    If condition trueBranch falseBranch ->
      containsHardcodedList condition || containsHardcodedList trueBranch || containsHardcodedList falseBranch
    List _ _ ->
      True
    Length listExpr ->
      containsHardcodedList listExpr
    Sum listExpr ->
      containsHardcodedList listExpr

exprHasHole :: Expr -> Bool
exprHasHole expr =
  case expr of
    Var _ _ ->
      False
    Hole _ _ ->
      True
    LitInt _ ->
      False
    LitBool _ ->
      False
    Add left right ->
      exprHasHole left || exprHasHole right
    Mul left right ->
      exprHasHole left || exprHasHole right
    Eq left right ->
      exprHasHole left || exprHasHole right
    If condition trueBranch falseBranch ->
      exprHasHole condition || exprHasHole trueBranch || exprHasHole falseBranch
    List items _ ->
      any exprHasHole items
    Length listExpr ->
      exprHasHole listExpr
    Sum listExpr ->
      exprHasHole listExpr

scoreExprLLM :: Env -> Type -> Expr -> IO ScoredExpr
scoreExprLLM =
  scoreExprLLMWithContext SearchScoring

scoreExprLLMWithContext :: ScoreContext -> Env -> Type -> Expr -> IO ScoredExpr
scoreExprLLMWithContext context env goalType expr =
  if exprType expr /= goalType
    then return wrongTypeScore
    else do
      maybeScore <- scoreWithLLM (candidatePrompt context env goalType expr)
      case maybeScore of
        Nothing ->
          return fallbackScore
        Just llmScore ->
          return
            ScoredExpr
              { scoredExpr = expr
              , scoredScore = llmScore
              , scoredReason = "type matches goal, scored by LM Studio"
              , scoredSource = "llm"
              , scoredRawLLMScore = Just llmScore
              , scoredFakeScore = scoredScore fakeScore
              , scoredBlended = False
              , scoredContext = scoreContextName context
              , scoredTaskName = Nothing
              , scoredByteModelScore = Nothing
              , scoredEngramScore = Nothing
              , scoredByteEngramScore = Nothing
              , scoredFinalScore = llmScore
              , scoredOptimizer = Nothing
              , scoredModelPath = Nothing
              , scoredEngramPath = Nothing
              }
  where
    fakeScore :: ScoredExpr
    fakeScore =
      scoreExprFakeWithContext context goalType expr

    fallbackScore :: ScoredExpr
    fallbackScore =
      scoreExprFallbackWithContext context goalType expr

    wrongTypeScore :: ScoredExpr
    wrongTypeScore =
      fakeScore
        { scoredScore = -1000.0
        , scoredReason = "wrong type, skipped LLM"
        , scoredSource = "fallback"
        , scoredFinalScore = -1000.0
        }

scoreExprLLMForTask :: ScoreContext -> TaskSpec -> Expr -> IO ScoredExpr
scoreExprLLMForTask context task expr =
  if exprType expr /= taskGoalType task
    then return wrongTypeScore
    else do
      maybeScore <- scoreWithLLM (candidatePromptForTask context task expr)
      case maybeScore of
        Nothing ->
          return fallbackScore
        Just llmScore ->
          return
            ScoredExpr
              { scoredExpr = expr
              , scoredScore = llmScore + 0.25 * scoredFakeScore fakeScore
              , scoredReason =
                  "type matches task goal, scored by LM Studio for task "
                    ++ taskName task
                    ++ ", final score blends raw LLM with 25% task fake score"
              , scoredSource = "llm-task-blend"
              , scoredRawLLMScore = Just llmScore
              , scoredFakeScore = scoredFakeScore fakeScore
              , scoredBlended = True
              , scoredContext = scoreContextName context
              , scoredTaskName = Just (taskName task)
              , scoredByteModelScore = Nothing
              , scoredEngramScore = Nothing
              , scoredByteEngramScore = Nothing
              , scoredFinalScore = llmScore + 0.25 * scoredFakeScore fakeScore
              , scoredOptimizer = Nothing
              , scoredModelPath = Nothing
              , scoredEngramPath = Nothing
              }
  where
    fakeScore :: ScoredExpr
    fakeScore =
      scoreExprFakeForTask context task expr

    fallbackScore :: ScoredExpr
    fallbackScore =
      fakeScore
        { scoredReason = scoredReason fakeScore ++ ", LLM unavailable"
        , scoredSource = "fallback"
        }

    wrongTypeScore :: ScoredExpr
    wrongTypeScore =
      fakeScore
        { scoredScore = -1000.0
        , scoredReason = "wrong type, skipped LLM for task " ++ taskName task
        , scoredSource = "fallback"
        , scoredFinalScore = -1000.0
        }

scoreExprBatchLLMWithContext :: ScoreContext -> Env -> Type -> [Expr] -> IO [ScoredExpr]
scoreExprBatchLLMWithContext context env goalType exprs = do
  config <- readLLMConfig
  scoredValues <-
    mapConcurrentlyBounded
      (llmParallelRequests config)
      (scoreExprLLMWithContext context env goalType)
      exprs
  if tooManyZeroLLMScores goalType scoredValues
    then do
      hPutStrLn stderr
        ( "Warning: LLM scoring returned 0.0 for more than 70% of "
            ++ scoreContextName context
            ++ " candidates; blending with fake scores."
        )
      return (map blendWithFakeScore scoredValues)
    else return scoredValues

scoreExprBatchLLMForTask :: ScoreContext -> TaskSpec -> [Expr] -> IO [ScoredExpr]
scoreExprBatchLLMForTask context task exprs = do
  config <- readLLMConfig
  scoredValues <-
    mapConcurrentlyBounded
      (llmParallelRequests config)
      (scoreExprLLMForTask context task)
      exprs
  if tooManyZeroLLMScores (taskGoalType task) scoredValues
    then
      hPutStrLn stderr
        ( "Warning: LLM task scoring returned 0.0 for more than 70% of "
            ++ scoreContextName context
            ++ " candidates for "
            ++ taskName task
            ++ "; using task-blended final scores."
        )
    else return ()
  return scoredValues

mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded limit action items = do
  semaphore <- newQSem (max 1 limit)
  resultVars <- mapM (startWorker semaphore action) items
  results <- mapM takeMVar resultVars
  collectResults results

startWorker :: QSem -> (a -> IO b) -> a -> IO (MVar (Either SomeException b))
startWorker semaphore action item = do
  resultVar <- newEmptyMVar
  _ <-
    forkFinally
      (bracket_ (waitQSem semaphore) (signalQSem semaphore) (action item))
      (putMVar resultVar)
  return resultVar

collectResults :: [Either SomeException a] -> IO [a]
collectResults results =
  case firstException results of
    Just exception ->
      throwIO exception
    Nothing ->
      return [ value | Right value <- results ]

firstException :: [Either SomeException a] -> Maybe SomeException
firstException results =
  case results of
    [] ->
      Nothing
    Left exception : _ ->
      Just exception
    Right _ : rest ->
      firstException rest

tooManyZeroLLMScores :: Type -> [ScoredExpr] -> Bool
tooManyZeroLLMScores goalType scoredValues =
  llmCount > 0 && zeroCount * 100 > llmCount * 70
  where
    sameTypeLLMScores :: [Double]
    sameTypeLLMScores =
      [ rawScore
      | scored <- scoredValues
      , exprType (scoredExpr scored) == goalType
      , Just rawScore <- [scoredRawLLMScore scored]
      ]

    llmCount :: Int
    llmCount =
      length sameTypeLLMScores

    zeroCount :: Int
    zeroCount =
      length (filter (== 0.0) sameTypeLLMScores)

blendWithFakeScore :: ScoredExpr -> ScoredExpr
blendWithFakeScore scored =
  case scoredRawLLMScore scored of
    Nothing ->
      scored
    Just rawScore ->
      if scoredFakeScore scored > 0.0
        then
          scored
            { scoredScore = rawScore + 0.25 * scoredFakeScore scored
            , scoredSource = "llm-blend"
            , scoredReason =
                scoredReason scored
                  ++ ", LLM zero-heavy batch; final score blends raw LLM with 25% fake score"
            , scoredBlended = True
            , scoredFinalScore = rawScore + 0.25 * scoredFakeScore scored
            }
        else scored
