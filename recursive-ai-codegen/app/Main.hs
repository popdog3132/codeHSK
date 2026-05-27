module Main where

import AST
import DataSet
import FunctionGen
import Generate
import Holes
import LLM
import Memory
import Pretty
import Score
import Task
import TestRunner
import TrainLog
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  let mode = parseMode progName args
  putStrLn ("Active mode: " ++ mode)
  putStrLn ("Training log: " ++ trainingLogPath)

  mapM_ (runGoal mode defaultEnv searchDepth) defaultGoals
  runHoleDemo mode defaultEnv holeExpansionSteps TInt
  runFunctionDemo mode

  putStrLn ""
  putStrLn ("Appended training rows to " ++ trainingLogPath)

parseMode :: String -> [String] -> String
parseMode progName args =
  case args of
    [] ->
      modeFromProgramName progName
    ["fake"] ->
      "fake"
    ["llm"] ->
      "llm"
    _ ->
      "fake"

modeFromProgramName :: String -> String
modeFromProgramName progName =
  case lastPathPart progName of
    "llm" ->
      "llm"
    _ ->
      "fake"

lastPathPart :: String -> String
lastPathPart path =
  case breakOnSlash path of
    Nothing ->
      path
    Just rest ->
      lastPathPart rest

breakOnSlash :: String -> Maybe String
breakOnSlash text =
  case text of
    [] ->
      Nothing
    '/' : rest ->
      Just rest
    _ : rest ->
      breakOnSlash rest

runGoal :: String -> Env -> Int -> Type -> IO ()
runGoal mode env depth goalType = do
  result <- runSearch mode env depth goalType
  putStrLn ""
  putStrLn ("Goal: " ++ prettyType goalType)
  printSearchResult result
  appendBestTrainingRow mode env trainingLogPath goalType result

runSearch :: String -> Env -> Int -> Type -> IO SearchResult
runSearch mode env depth goalType =
  case mode of
    "llm" ->
      searchIO env depth goalType
    _ ->
      return (search env depth goalType)

printSearchResult :: SearchResult -> IO ()
printSearchResult result = do
  putStrLn "Top 10 candidates:"
  mapM_ printScoredExpr (take 10 (searchCandidates result))
  putStrLn "Best expression:"
  case searchBest result of
    Nothing ->
      putStrLn "  <none>"
    Just best ->
      putStrLn
        ( "  "
            ++ prettyExpr (scoredExpr best)
            ++ " :: "
            ++ prettyType (exprType (scoredExpr best))
        )

printScoredExpr :: ScoredExpr -> IO ()
printScoredExpr scored =
  putStrLn
    ( "  "
        ++ prettyExpr (scoredExpr scored)
        ++ "    score="
        ++ show (scoredScore scored)
        ++ "    source="
        ++ scoredSource scored
        ++ "    "
        ++ scoredReason scored
    )

appendBestTrainingRow :: String -> Env -> FilePath -> Type -> SearchResult -> IO ()
appendBestTrainingRow mode env path goalType result =
  case searchBest result of
    Nothing ->
      return ()
    Just best ->
      appendTrainingRow path (trainingRow mode env goalType (searchCandidates result) best)

holeExpansionSteps :: Int
holeExpansionSteps =
  6

runHoleDemo :: String -> Env -> Int -> Type -> IO ()
runHoleDemo mode env maxSteps goalType = do
  putStrLn ""
  putStrLn ("Hole expansion demo: " ++ prettyType goalType)
  putStrLn ("Step 0: " ++ prettyExpr startExpr)
  finalExpr <- expandWithLogging mode env goalType maxSteps 1 startExpr
  putStrLn ("Final hole expression: " ++ prettyExpr finalExpr)
  putStrLn ("Final type: " ++ prettyType (exprType finalExpr))
  where
    startExpr :: Expr
    startExpr =
      initialHole goalType

expandWithLogging :: String -> Env -> Type -> Int -> Int -> Expr -> IO Expr
expandWithLogging mode env goalType stepsLeft stepNumber expr =
  if stepsLeft <= 0 || not (hasHoles expr)
    then return expr
    else do
      maybeDecision <- nextExpansionDecision mode env expr
      case maybeDecision of
        Nothing ->
          return expr
        Just decision -> do
          appendTrainingRow trainingLogPath (expansionRow mode env goalType decision)
          printExpansionDecision stepNumber decision
          expandWithLogging mode env goalType (stepsLeft - 1) (stepNumber + 1) (expansionNewExpr decision)

nextExpansionDecision :: String -> Env -> Expr -> IO (Maybe ExpansionDecision)
nextExpansionDecision mode env expr =
  case mode of
    "llm" ->
      expandStepLLMDecision env expr
    _ ->
      return (expandStepFakeDecision env expr)

printExpansionDecision :: Int -> ExpansionDecision -> IO ()
printExpansionDecision stepNumber decision =
  putStrLn
    ( "Step "
        ++ show stepNumber
        ++ ": hole "
        ++ show (expansionHoleId decision)
        ++ " :: "
        ++ prettyType (expansionHoleType decision)
        ++ " -> "
        ++ prettyExpr (expansionChosenReplacement decision)
        ++ "    score="
        ++ show (scoredScore (expansionChosenScore decision))
        ++ "    source="
        ++ scoredSource (expansionChosenScore decision)
        ++ "    origin="
        ++ expansionChosenOrigin decision
        ++ memoryPatternText (expansionChosenPatternName decision)
    )
    >> putStrLn ("        " ++ prettyExpr (expansionNewExpr decision))

memoryPatternText :: String -> String
memoryPatternText name =
  if null name then "" else "    pattern=" ++ name

runFunctionDemo :: String -> IO ()
runFunctionDemo mode = do
  putStrLn ""
  runFunctionTask mode sumTask "generatedSum :: [Int] -> Int" generatedSumSpec generatedSumSourcePath generatedSumTests
  putStrLn ""
  runFunctionTask mode lengthTask "generatedLength :: [Int] -> Int" generatedLengthSpec generatedLengthSourcePath generatedLengthTests

runFunctionTask :: String -> TaskSpec -> String -> (Expr -> FunctionSpec) -> FilePath -> [TestCase] -> IO ()
runFunctionTask mode task label makeSpec sourcePath tests = do
  putStrLn ("Function generation demo: " ++ label)
  printAvailableMemoryPatterns (taskEnv task) (taskGoalType task)
  holeBody <- bodyFromTaskHoleExpansion mode task functionExpansionSteps
  putStrLn ("Initial hole-generated body: " ++ prettyExpr holeBody)
  selectedSpec <- chooseFunctionSpecWithFeedback mode task (makeSpec holeBody) sourcePath tests
  let source = functionSourceWithTests selectedSpec tests
  putStrLn "Generated function source:"
  putStrLn source
  result <- runFunctionTests sourcePath source
  putStrLn ("Function tests: " ++ if testPassed result then "PASS" else "FAIL")
  printTestOutput result
  bodyScore <- scoreFunctionBody mode task (functionBody selectedSpec)
  appendTrainingRow trainingLogPath (functionTestRowForTask mode task selectedSpec sourcePath source tests result bodyScore)

printAvailableMemoryPatterns :: Env -> Type -> IO ()
printAvailableMemoryPatterns env goalType =
  case patternsForGoal env goalType of
    [] ->
      putStrLn "Memory patterns: <none>"
    patterns ->
      putStrLn ("Memory patterns: " ++ commaList (map patternName patterns))

bodyFromTaskHoleExpansion :: String -> TaskSpec -> Int -> IO Expr
bodyFromTaskHoleExpansion mode task maxSteps =
  expandTaskWithLogging mode task maxSteps 1 (initialHole (taskGoalType task))

expandTaskWithLogging :: String -> TaskSpec -> Int -> Int -> Expr -> IO Expr
expandTaskWithLogging mode task stepsLeft stepNumber expr =
  if stepsLeft <= 0 || not (hasHoles expr)
    then return expr
    else do
      maybeDecision <- nextTaskExpansionDecision mode task expr
      case maybeDecision of
        Nothing ->
          return expr
        Just decision -> do
          appendTrainingRow trainingLogPath (expansionRowForTask mode task decision)
          printTaskExpansionDecision stepNumber task decision
          expandTaskWithLogging mode task (stepsLeft - 1) (stepNumber + 1) (expansionNewExpr decision)

nextTaskExpansionDecision :: String -> TaskSpec -> Expr -> IO (Maybe ExpansionDecision)
nextTaskExpansionDecision mode task expr =
  case mode of
    "llm" ->
      expandStepLLMDecisionForTask task expr
    _ ->
      return (expandStepFakeDecisionForTask task expr)

printTaskExpansionDecision :: Int -> TaskSpec -> ExpansionDecision -> IO ()
printTaskExpansionDecision stepNumber task decision =
  putStrLn
    ( "Task step "
        ++ show stepNumber
        ++ " ("
        ++ taskName task
        ++ "): hole "
        ++ show (expansionHoleId decision)
        ++ " :: "
        ++ prettyType (expansionHoleType decision)
        ++ " -> "
        ++ prettyExpr (expansionChosenReplacement decision)
        ++ "    score="
        ++ show (scoredScore (expansionChosenScore decision))
        ++ "    source="
        ++ scoredSource (expansionChosenScore decision)
        ++ "    origin="
        ++ expansionChosenOrigin decision
        ++ memoryPatternText (expansionChosenPatternName decision)
    )
    >> putStrLn ("        " ++ prettyExpr (expansionNewExpr decision))

chooseFunctionSpecWithFeedback :: String -> TaskSpec -> FunctionSpec -> FilePath -> [TestCase] -> IO FunctionSpec
chooseFunctionSpecWithFeedback mode task initialSpec sourcePath tests = do
  initialResult <- runFunctionTests sourcePath (functionSourceWithTests initialSpec tests)
  candidates <- candidateFunctionSpecsForTask mode task initialSpec
  if testPassed initialResult
    then do
      maybeSimplerSpec <- firstPassingCandidate sourcePath tests candidates
      case maybeSimplerSpec of
        Nothing ->
          return initialSpec
        Just simplerSpec ->
          if exprSize (functionBody simplerSpec) <= exprSize (functionBody initialSpec)
            then do
              putStrLn "A smaller complete candidate passed tests; using it."
              return simplerSpec
            else return initialSpec
    else do
      putStrLn "Initial body did not pass tests; trying complete generated candidates."
      maybeCandidate <- firstPassingCandidate sourcePath tests candidates
      case maybeCandidate of
        Nothing ->
          return initialSpec
        Just candidateSpec ->
          return candidateSpec

firstPassingCandidate :: FilePath -> [TestCase] -> [FunctionSpec] -> IO (Maybe FunctionSpec)
firstPassingCandidate sourcePath tests candidates =
  case candidates of
    [] ->
      return Nothing
    candidateSpec : rest -> do
      result <- runFunctionTests sourcePath (functionSourceWithTests candidateSpec tests)
      if testPassed result
        then return (Just candidateSpec)
        else firstPassingCandidate sourcePath tests rest

candidateFunctionSpecsForTask :: String -> TaskSpec -> FunctionSpec -> IO [FunctionSpec]
candidateFunctionSpecsForTask mode task spec = do
  scoredBodies <- scoreFunctionCandidateBodies mode task completeBodies
  return (map makeSpec (map scoredExpr (sortScored scoredBodies)))
  where
    completeBodies :: [Expr]
    completeBodies =
      uniqueExprs
        ( filter (not . hasHoles)
            ( patternCandidates (taskEnv task) (taskGoalType task)
                ++ allCandidates (taskEnv task) 1 (taskGoalType task)
            )
        )

    makeSpec :: Expr -> FunctionSpec
    makeSpec body =
      spec { functionBody = body }

scoreFunctionCandidateBodies :: String -> TaskSpec -> [Expr] -> IO [ScoredExpr]
scoreFunctionCandidateBodies mode task bodies =
  case mode of
    "llm" -> do
      canUseLLM <- canUseLLMScoring
      if canUseLLM
        then scoreExprBatchLLMForTask FunctionBodyScoring task bodies
        else return (map (scoreExprFakeForTask FunctionBodyScoring task) bodies)
    _ ->
      return (map (scoreExprFakeForTask FunctionBodyScoring task) bodies)

scoreFunctionBody :: String -> TaskSpec -> Expr -> IO ScoredExpr
scoreFunctionBody mode task body =
  case mode of
    "llm" -> do
      canUseLLM <- canUseLLMScoring
      if canUseLLM
        then scoreExprLLMForTask FunctionBodyScoring task body
        else return (scoreExprFakeForTask FunctionBodyScoring task body)
    _ ->
      return (scoreExprFakeForTask FunctionBodyScoring task body)

sortScored :: [ScoredExpr] -> [ScoredExpr]
sortScored scoredValues =
  case scoredValues of
    [] ->
      []
    pivot : rest ->
      sortScored better ++ [pivot] ++ sortScored worse
      where
        better :: [ScoredExpr]
        better =
          [ scored
          | scored <- rest
          , scoredScore scored > scoredScore pivot
          ]

        worse :: [ScoredExpr]
        worse =
          [ scored
          | scored <- rest
          , scoredScore scored <= scoredScore pivot
          ]

uniqueExprs :: [Expr] -> [Expr]
uniqueExprs exprs =
  case exprs of
    [] ->
      []
    expr : rest ->
      expr : uniqueExprs (filter (/= expr) rest)

printTestOutput :: TestResult -> IO ()
printTestOutput result = do
  if null (testStdout result)
    then return ()
    else putStrLn ("stdout:\n" ++ testStdout result)
  if null (testStderr result)
    then return ()
    else putStrLn ("stderr:\n" ++ testStderr result)

generatedSumSpec :: Expr -> FunctionSpec
generatedSumSpec body =
  FunctionSpec
    { functionName = "generatedSum"
    , functionArgs = generatedListEnv
    , functionReturnType = TInt
    , functionBody = body
    }

generatedLengthSpec :: Expr -> FunctionSpec
generatedLengthSpec body =
  FunctionSpec
    { functionName = "generatedLength"
    , functionArgs = generatedListEnv
    , functionReturnType = TInt
    , functionBody = body
    }

generatedListEnv :: Env
generatedListEnv =
  [("nums", TList TInt)]

generatedSumTests :: [TestCase]
generatedSumTests =
  [ TestCase "generatedSum []" "0"
  , TestCase "generatedSum [1,2,3]" "6"
  , TestCase "generatedSum [10,-2,5]" "13"
  ]

generatedSumSourcePath :: FilePath
generatedSumSourcePath =
  "data/GeneratedSumDemo.hs"

generatedLengthTests :: [TestCase]
generatedLengthTests =
  [ TestCase "generatedLength []" "0"
  , TestCase "generatedLength [1,2,3]" "3"
  , TestCase "generatedLength [10,-2,5,7]" "4"
  ]

generatedLengthSourcePath :: FilePath
generatedLengthSourcePath =
  "data/GeneratedLengthDemo.hs"

functionExpansionSteps :: Int
functionExpansionSteps =
  6

commaList :: [String] -> String
commaList values =
  case values of
    [] ->
      ""
    first : rest ->
      first ++ concatMap (", " ++) rest
