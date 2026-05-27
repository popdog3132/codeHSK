module TrainLog
  ( trainingRow
  , expansionRow
  , expansionRowForTask
  , functionTestRow
  , functionTestRowForTask
  , appendTrainingRow
  ) where

import AST
import DataSet
import FunctionGen
import Holes
import Memory
import Pretty
import Score
import Task
import TestRunner
import Data.List (intercalate)

trainingRow :: String -> Env -> Type -> [ScoredExpr] -> ScoredExpr -> String
trainingRow mode env goalType scoredCandidates best =
  "{"
    ++ "\"kind\":\"search\","
    ++ "\"mode\":" ++ jsonString mode ++ ","
    ++ "\"task_name\":" ++ jsonMaybeString (scoredTaskName best) ++ ","
    ++ "\"scoring_context\":\"search\","
    ++ "\"goal_type\":" ++ jsonString (prettyType goalType) ++ ","
    ++ "\"environment\":[" ++ intercalate "," (map envBindingJson env) ++ "],"
    ++ "\"chosen_expression\":" ++ jsonString (prettyExpr (scoredExpr best)) ++ ","
    ++ "\"best_expression\":" ++ jsonString (prettyExpr (scoredExpr best)) ++ ","
    ++ "\"best_score\":" ++ show (scoredScore best) ++ ","
    ++ "\"best_score_source\":" ++ jsonString (scoredSource best) ++ ","
    ++ "\"raw_llm_score\":" ++ jsonMaybeNumber (scoredRawLLMScore best) ++ ","
    ++ "\"byte_model_score\":" ++ jsonMaybeNumber (scoredByteModelScore best) ++ ","
    ++ "\"engram_score\":" ++ jsonMaybeNumber (scoredEngramScore best) ++ ","
    ++ "\"byte_engram_score\":" ++ jsonMaybeNumber (scoredByteEngramScore best) ++ ","
    ++ "\"fake_score\":" ++ show (scoredFakeScore best) ++ ","
    ++ "\"final_score\":" ++ show (scoredFinalScore best) ++ ","
    ++ "\"optimizer\":" ++ jsonMaybeString (scoredOptimizer best) ++ ","
    ++ "\"model_path\":" ++ jsonMaybeString (scoredModelPath best) ++ ","
    ++ "\"engram_path\":" ++ jsonMaybeString (scoredEngramPath best) ++ ","
    ++ "\"top_candidates\":["
    ++ intercalate "," (map scoredCandidateJson (take 10 scoredCandidates))
    ++ "]"
    ++ "}"

appendTrainingRow :: FilePath -> String -> IO ()
appendTrainingRow path row =
  appendFile path (row ++ "\n")

expansionRow :: String -> Env -> Type -> ExpansionDecision -> String
expansionRow mode env goalType decision =
  expansionRowWithTask mode Nothing env goalType decision

expansionRowForTask :: String -> TaskSpec -> ExpansionDecision -> String
expansionRowForTask mode task decision =
  expansionRowWithTask mode (Just task) (taskEnv task) (taskGoalType task) decision

expansionRowWithTask :: String -> Maybe TaskSpec -> Env -> Type -> ExpansionDecision -> String
expansionRowWithTask mode maybeTask env goalType decision =
  "{"
    ++ "\"kind\":\"hole_expansion\","
    ++ "\"mode\":" ++ jsonString mode ++ ","
    ++ taskFields maybeTask
    ++ "\"scoring_context\":\"hole_expansion\","
    ++ "\"goal_type\":" ++ jsonString (prettyType goalType) ++ ","
    ++ "\"environment\":[" ++ intercalate "," (map envBindingJson env) ++ "],"
    ++ "\"original_partial_expression\":"
    ++ jsonString (prettyExpr (expansionOriginal decision)) ++ ","
    ++ "\"selected_hole_id\":" ++ show (expansionHoleId decision) ++ ","
    ++ "\"selected_hole_type\":"
    ++ jsonString (prettyType (expansionHoleType decision)) ++ ","
    ++ "\"memory_patterns_available\":["
    ++ intercalate "," (map jsonString (expansionMemoryPatternNames decision))
    ++ "],"
    ++ "\"candidates\":["
    ++ intercalate "," (map expansionCandidateJson (expansionCandidates decision))
    ++ "],"
    ++ "\"chosen_replacement\":"
    ++ jsonString (prettyExpr (expansionChosenReplacement decision)) ++ ","
    ++ "\"chosen_origin\":" ++ jsonString (expansionChosenOrigin decision) ++ ","
    ++ "\"chosen_memory_pattern\":"
    ++ jsonString (expansionChosenPatternName decision) ++ ","
    ++ "\"new_partial_expression\":"
    ++ jsonString (prettyExpr (expansionNewExpr decision)) ++ ","
    ++ "\"chosen_score\":" ++ show (scoredScore (expansionChosenScore decision)) ++ ","
    ++ "\"raw_llm_score\":"
    ++ jsonMaybeNumber (scoredRawLLMScore (expansionChosenScore decision)) ++ ","
    ++ "\"byte_model_score\":"
    ++ jsonMaybeNumber (scoredByteModelScore (expansionChosenScore decision)) ++ ","
    ++ "\"engram_score\":"
    ++ jsonMaybeNumber (scoredEngramScore (expansionChosenScore decision)) ++ ","
    ++ "\"byte_engram_score\":"
    ++ jsonMaybeNumber (scoredByteEngramScore (expansionChosenScore decision)) ++ ","
    ++ "\"fake_score\":"
    ++ show (scoredFakeScore (expansionChosenScore decision)) ++ ","
    ++ "\"final_score\":"
    ++ show (scoredFinalScore (expansionChosenScore decision)) ++ ","
    ++ "\"optimizer\":"
    ++ jsonMaybeString (scoredOptimizer (expansionChosenScore decision)) ++ ","
    ++ "\"model_path\":"
    ++ jsonMaybeString (scoredModelPath (expansionChosenScore decision)) ++ ","
    ++ "\"engram_path\":"
    ++ jsonMaybeString (scoredEngramPath (expansionChosenScore decision)) ++ ","
    ++ "\"score_source\":"
    ++ jsonString (scoredSource (expansionChosenScore decision)) ++ ","
    ++ "\"chosen_score_source\":"
    ++ jsonString (scoredSource (expansionChosenScore decision))
    ++ "}"

functionTestRow :: String -> FunctionSpec -> FilePath -> String -> [TestCase] -> TestResult -> String
functionTestRow mode spec sourcePath source tests result =
  functionTestRowWithTask mode Nothing spec sourcePath source tests result bodyScore
  where
    bodyScore :: ScoredExpr
    bodyScore =
      scoreExprFakeWithContext FunctionBodyScoring (functionReturnType spec) (functionBody spec)

functionTestRowForTask :: String -> TaskSpec -> FunctionSpec -> FilePath -> String -> [TestCase] -> TestResult -> ScoredExpr -> String
functionTestRowForTask mode task spec sourcePath source tests result bodyScore =
  functionTestRowWithTask mode (Just task) spec sourcePath source tests result bodyScore

functionTestRowWithTask :: String -> Maybe TaskSpec -> FunctionSpec -> FilePath -> String -> [TestCase] -> TestResult -> ScoredExpr -> String
functionTestRowWithTask mode maybeTask spec sourcePath source tests result bodyScore =
  "{"
    ++ "\"kind\":\"function_test\","
    ++ "\"mode\":" ++ jsonString mode ++ ","
    ++ taskFields maybeTask
    ++ "\"function_name\":" ++ jsonString (functionName spec) ++ ","
    ++ "\"function_type\":" ++ jsonString (functionTypeText spec) ++ ","
    ++ "\"scoring_context\":\"function_body\","
    ++ "\"generated_body\":" ++ jsonString (prettyExpr (functionBody spec)) ++ ","
    ++ "\"raw_llm_score\":" ++ jsonMaybeNumber (scoredRawLLMScore bodyScore) ++ ","
    ++ "\"byte_model_score\":" ++ jsonMaybeNumber (scoredByteModelScore bodyScore) ++ ","
    ++ "\"engram_score\":" ++ jsonMaybeNumber (scoredEngramScore bodyScore) ++ ","
    ++ "\"byte_engram_score\":" ++ jsonMaybeNumber (scoredByteEngramScore bodyScore) ++ ","
    ++ "\"fake_score\":" ++ show (scoredFakeScore bodyScore) ++ ","
    ++ "\"final_score\":" ++ show (scoredFinalScore bodyScore) ++ ","
    ++ "\"final_blended_score\":" ++ show (scoredScore bodyScore) ++ ","
    ++ "\"score_source\":" ++ jsonString (scoredSource bodyScore) ++ ","
    ++ "\"blending_used\":" ++ jsonBool (scoredBlended bodyScore) ++ ","
    ++ "\"optimizer\":" ++ jsonMaybeString (scoredOptimizer bodyScore) ++ ","
    ++ "\"model_path\":" ++ jsonMaybeString (scoredModelPath bodyScore) ++ ","
    ++ "\"engram_path\":" ++ jsonMaybeString (scoredEngramPath bodyScore) ++ ","
    ++ "\"generated_source_path\":" ++ jsonString sourcePath ++ ","
    ++ "\"generated_function_source\":" ++ jsonString source ++ ","
    ++ "\"memory_patterns_available\":["
    ++ intercalate "," (map jsonString functionMemoryPatternNames)
    ++ "],"
    ++ "\"tests\":[" ++ intercalate "," (map testCaseJson tests) ++ "],"
    ++ "\"testPassed\":" ++ jsonBool (testPassed result) ++ ","
    ++ "\"exit_code\":" ++ jsonString (testExitCode result) ++ ","
    ++ "\"stdout\":" ++ jsonString (testStdout result) ++ ","
    ++ "\"stderr\":" ++ jsonString (testStderr result)
    ++ "}"
  where
    functionMemoryPatternNames :: [String]
    functionMemoryPatternNames =
      map patternName (patternsForGoal (functionArgs spec) (functionReturnType spec))

scoredCandidateJson :: ScoredExpr -> String
scoredCandidateJson scored =
  "{"
    ++ "\"expression\":" ++ jsonString (prettyExpr (scoredExpr scored)) ++ ","
    ++ "\"score\":" ++ show (scoredScore scored) ++ ","
    ++ "\"raw_llm_score\":" ++ jsonMaybeNumber (scoredRawLLMScore scored) ++ ","
    ++ "\"byte_model_score\":" ++ jsonMaybeNumber (scoredByteModelScore scored) ++ ","
    ++ "\"engram_score\":" ++ jsonMaybeNumber (scoredEngramScore scored) ++ ","
    ++ "\"byte_engram_score\":" ++ jsonMaybeNumber (scoredByteEngramScore scored) ++ ","
    ++ "\"fake_score\":" ++ show (scoredFakeScore scored) ++ ","
    ++ "\"final_score\":" ++ show (scoredFinalScore scored) ++ ","
    ++ "\"final_blended_score\":" ++ show (scoredScore scored) ++ ","
    ++ "\"blending_used\":" ++ jsonBool (scoredBlended scored) ++ ","
    ++ "\"scoring_context\":" ++ jsonString (scoredContext scored) ++ ","
    ++ "\"task_name\":" ++ jsonMaybeString (scoredTaskName scored) ++ ","
    ++ "\"optimizer\":" ++ jsonMaybeString (scoredOptimizer scored) ++ ","
    ++ "\"model_path\":" ++ jsonMaybeString (scoredModelPath scored) ++ ","
    ++ "\"engram_path\":" ++ jsonMaybeString (scoredEngramPath scored) ++ ","
    ++ "\"score_source\":" ++ jsonString (scoredSource scored) ++ ","
    ++ "\"reason\":" ++ jsonString (scoredReason scored)
    ++ "}"

expansionCandidateJson :: (ReplacementCandidate, ScoredExpr) -> String
expansionCandidateJson (candidate, scored) =
  "{"
    ++ "\"replacement\":" ++ jsonString (prettyExpr (replacementExpr candidate)) ++ ","
    ++ "\"origin\":" ++ jsonString (replacementOrigin candidate) ++ ","
    ++ "\"memory_pattern\":"
    ++ jsonString (replacementPatternName candidate) ++ ","
    ++ "\"result_expression\":" ++ jsonString (prettyExpr (scoredExpr scored)) ++ ","
    ++ "\"score\":" ++ show (scoredScore scored) ++ ","
    ++ "\"raw_llm_score\":" ++ jsonMaybeNumber (scoredRawLLMScore scored) ++ ","
    ++ "\"byte_model_score\":" ++ jsonMaybeNumber (scoredByteModelScore scored) ++ ","
    ++ "\"engram_score\":" ++ jsonMaybeNumber (scoredEngramScore scored) ++ ","
    ++ "\"byte_engram_score\":" ++ jsonMaybeNumber (scoredByteEngramScore scored) ++ ","
    ++ "\"fake_score\":" ++ show (scoredFakeScore scored) ++ ","
    ++ "\"final_score\":" ++ show (scoredFinalScore scored) ++ ","
    ++ "\"final_blended_score\":" ++ show (scoredScore scored) ++ ","
    ++ "\"blending_used\":" ++ jsonBool (scoredBlended scored) ++ ","
    ++ "\"scoring_context\":" ++ jsonString (scoredContext scored) ++ ","
    ++ "\"task_name\":" ++ jsonMaybeString (scoredTaskName scored) ++ ","
    ++ "\"optimizer\":" ++ jsonMaybeString (scoredOptimizer scored) ++ ","
    ++ "\"model_path\":" ++ jsonMaybeString (scoredModelPath scored) ++ ","
    ++ "\"engram_path\":" ++ jsonMaybeString (scoredEngramPath scored) ++ ","
    ++ "\"score_source\":" ++ jsonString (scoredSource scored) ++ ","
    ++ "\"reason\":" ++ jsonString (scoredReason scored)
    ++ "}"

testCaseJson :: TestCase -> String
testCaseJson test =
  "{"
    ++ "\"call\":" ++ jsonString (testCall test) ++ ","
    ++ "\"expected\":" ++ jsonString (expectedValue test)
    ++ "}"

envBindingJson :: (String, Type) -> String
envBindingJson (name, typeValue) =
  "{"
    ++ "\"name\":" ++ jsonString name ++ ","
    ++ "\"type\":" ++ jsonString (prettyType typeValue)
    ++ "}"

taskFields :: Maybe TaskSpec -> String
taskFields maybeTask =
  case maybeTask of
    Nothing ->
      "\"task_name\":null,\"task_description\":null,"
    Just task ->
      "\"task_name\":" ++ jsonString (taskName task) ++ ","
        ++ "\"task_description\":" ++ jsonString (taskDescription task) ++ ","

jsonString :: String -> String
jsonString text =
  "\"" ++ concatMap escapeJsonChar text ++ "\""

jsonBool :: Bool -> String
jsonBool value =
  if value then "true" else "false"

jsonMaybeNumber :: Maybe Double -> String
jsonMaybeNumber maybeNumber =
  case maybeNumber of
    Nothing ->
      "null"
    Just number ->
      show number

jsonMaybeString :: Maybe String -> String
jsonMaybeString maybeText =
  case maybeText of
    Nothing ->
      "null"
    Just text ->
      jsonString text

escapeJsonChar :: Char -> String
escapeJsonChar char =
  case char of
    '"' ->
      "\\\""
    '\\' ->
      "\\\\"
    '\n' ->
      "\\n"
    '\r' ->
      "\\r"
    '\t' ->
      "\\t"
    _ ->
      [char]
