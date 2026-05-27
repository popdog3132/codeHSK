module TrainLog
  ( trainingRow
  , expansionRow
  , functionTestRow
  , appendTrainingRow
  ) where

import AST
import DataSet
import FunctionGen
import Holes
import Memory
import Pretty
import Score
import TestRunner
import Data.List (intercalate)

trainingRow :: String -> Env -> Type -> [ScoredExpr] -> ScoredExpr -> String
trainingRow mode env goalType scoredCandidates best =
  "{"
    ++ "\"kind\":\"search\","
    ++ "\"mode\":" ++ jsonString mode ++ ","
    ++ "\"goal_type\":" ++ jsonString (prettyType goalType) ++ ","
    ++ "\"environment\":[" ++ intercalate "," (map envBindingJson env) ++ "],"
    ++ "\"chosen_expression\":" ++ jsonString (prettyExpr (scoredExpr best)) ++ ","
    ++ "\"best_expression\":" ++ jsonString (prettyExpr (scoredExpr best)) ++ ","
    ++ "\"best_score\":" ++ show (scoredScore best) ++ ","
    ++ "\"best_score_source\":" ++ jsonString (scoredSource best) ++ ","
    ++ "\"top_candidates\":["
    ++ intercalate "," (map scoredCandidateJson (take 10 scoredCandidates))
    ++ "]"
    ++ "}"

appendTrainingRow :: FilePath -> String -> IO ()
appendTrainingRow path row =
  appendFile path (row ++ "\n")

expansionRow :: String -> Env -> Type -> ExpansionDecision -> String
expansionRow mode env goalType decision =
  "{"
    ++ "\"kind\":\"hole_expansion\","
    ++ "\"mode\":" ++ jsonString mode ++ ","
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
    ++ "\"chosen_score_source\":"
    ++ jsonString (scoredSource (expansionChosenScore decision))
    ++ "}"

functionTestRow :: String -> FunctionSpec -> FilePath -> String -> [TestCase] -> TestResult -> String
functionTestRow mode spec sourcePath source tests result =
  "{"
    ++ "\"kind\":\"function_test\","
    ++ "\"mode\":" ++ jsonString mode ++ ","
    ++ "\"function_name\":" ++ jsonString (functionName spec) ++ ","
    ++ "\"function_type\":" ++ jsonString (functionTypeText spec) ++ ","
    ++ "\"generated_body\":" ++ jsonString (prettyExpr (functionBody spec)) ++ ","
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

jsonString :: String -> String
jsonString text =
  "\"" ++ concatMap escapeJsonChar text ++ "\""

jsonBool :: Bool -> String
jsonBool value =
  if value then "true" else "false"

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
