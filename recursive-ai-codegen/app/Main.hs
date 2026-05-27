module Main where

import AST
import DataSet
import FunctionGen
import Generate
import Holes
import Memory
import Pretty
import Score
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
    )
    >> putStrLn ("        " ++ prettyExpr (expansionNewExpr decision))

runFunctionDemo :: String -> IO ()
runFunctionDemo mode = do
  putStrLn ""
  runFunctionTask mode "generatedSum :: [Int] -> Int" generatedSumEnv TInt generatedSumSpec generatedSumSourcePath generatedSumTests
  putStrLn ""
  runFunctionTask mode "generatedLength :: [Int] -> Int" generatedListEnv TInt generatedLengthSpec generatedLengthSourcePath generatedLengthTests

runFunctionTask :: String -> String -> Env -> Type -> (Expr -> FunctionSpec) -> FilePath -> [TestCase] -> IO ()
runFunctionTask mode label env goalType makeSpec sourcePath tests = do
  putStrLn ("Function generation demo: " ++ label)
  printAvailableMemoryPatterns env goalType
  holeBody <- bodyFromHoleExpansion mode env functionExpansionSteps goalType
  putStrLn ("Initial hole-generated body: " ++ prettyExpr holeBody)
  selectedSpec <- chooseFunctionSpecWithFeedback (makeSpec holeBody) sourcePath tests
  let source = functionSourceWithTests selectedSpec tests
  putStrLn "Generated function source:"
  putStrLn source
  result <- runFunctionTests sourcePath source
  putStrLn ("Function tests: " ++ if testPassed result then "PASS" else "FAIL")
  printTestOutput result
  appendTrainingRow trainingLogPath (functionTestRow mode selectedSpec sourcePath source tests result)

printAvailableMemoryPatterns :: Env -> Type -> IO ()
printAvailableMemoryPatterns env goalType =
  case patternsForGoal env goalType of
    [] ->
      putStrLn "Memory patterns: <none>"
    patterns ->
      putStrLn ("Memory patterns: " ++ commaList (map patternName patterns))

bodyFromHoleExpansion :: String -> Env -> Int -> Type -> IO Expr
bodyFromHoleExpansion mode env maxSteps goalType =
  case mode of
    "llm" ->
      expandFullyLLM env maxSteps goalType
    _ ->
      return (expandFullyFake env maxSteps goalType)

chooseFunctionSpecWithFeedback :: FunctionSpec -> FilePath -> [TestCase] -> IO FunctionSpec
chooseFunctionSpecWithFeedback initialSpec sourcePath tests = do
  initialResult <- runFunctionTests sourcePath (functionSourceWithTests initialSpec tests)
  if testPassed initialResult
    then return initialSpec
    else do
      putStrLn "Initial body did not pass tests; trying complete generated candidates."
      tryCandidateSpecs initialSpec sourcePath tests (candidateFunctionSpecs initialSpec)

tryCandidateSpecs :: FunctionSpec -> FilePath -> [TestCase] -> [FunctionSpec] -> IO FunctionSpec
tryCandidateSpecs fallbackSpec sourcePath tests candidates =
  case candidates of
    [] ->
      return fallbackSpec
    candidateSpec : rest -> do
      result <- runFunctionTests sourcePath (functionSourceWithTests candidateSpec tests)
      if testPassed result
        then return candidateSpec
        else tryCandidateSpecs fallbackSpec sourcePath tests rest

candidateFunctionSpecs :: FunctionSpec -> [FunctionSpec]
candidateFunctionSpecs spec =
  map makeSpec (uniqueExprs completeBodies)
  where
    completeBodies :: [Expr]
    completeBodies =
      filter (not . hasHoles)
        ( patternCandidates (functionArgs spec) (functionReturnType spec)
            ++ allCandidates (functionArgs spec) 1 (functionReturnType spec)
        )

    makeSpec :: Expr -> FunctionSpec
    makeSpec body =
      spec { functionBody = body }

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
