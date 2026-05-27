module Prompt
  ( candidatePrompt
  , candidatePromptForTask
  ) where

import AST
import DataSet
import Pretty
import ScoringContext
import Task

candidatePrompt :: ScoreContext -> Env -> Type -> Expr -> String
candidatePrompt context env goalType expr =
  "Goal type: " ++ prettyType goalType ++ "\n"
    ++ "Scoring context: " ++ scoreContextName context ++ "\n"
    ++ "Environment:\n"
    ++ concatMap envLine env
    ++ "\n"
    ++ "Candidate:\n"
    ++ prettyExpr expr ++ "\n\n"
    ++ "Return only JSON with one field named score.\n"
    ++ "The score field must be exactly one number from 0 to 100.\n"
    ++ "Do not output explanation, markdown, or extra fields.\n"
    ++ "Score based on usefulness for the stated goal and environment.\n"
    ++ "Do not score every unfamiliar or simple candidate as 0.\n"
    ++ "Use 0 only for wrong-type, impossible, or clearly useless candidates.\n"
    ++ "A type-correct but basic candidate should usually receive a nonzero score.\n"
    ++ contextGuidance context

candidatePromptForTask :: ScoreContext -> TaskSpec -> Expr -> String
candidatePromptForTask context task expr =
  "Task name: " ++ taskName task ++ "\n"
    ++ "Task description: " ++ taskDescription task ++ "\n"
    ++ "Goal type: " ++ prettyType (taskGoalType task) ++ "\n"
    ++ "Scoring context: " ++ scoreContextName context ++ "\n"
    ++ "Environment:\n"
    ++ concatMap envLine (taskEnv task)
    ++ "\n"
    ++ "Candidate expression:\n"
    ++ prettyExpr expr ++ "\n\n"
    ++ "Score the candidate for solving this specific task, not merely for having the correct type.\n"
    ++ "A complete expression that directly solves the task should score high.\n"
    ++ "A constant, unrelated variable, or expression using hardcoded list literals should score low unless it solves the task.\n"
    ++ "Return only JSON with one field named score.\n"
    ++ "The score field must be exactly one number from 0 to 100.\n"
    ++ "Do not output explanation, markdown, or extra fields.\n"
    ++ contextGuidance context

envLine :: (String, Type) -> String
envLine (name, typeValue) =
  name ++ " :: " ++ prettyType typeValue ++ "\n"

contextGuidance :: ScoreContext -> String
contextGuidance context =
  case context of
    SearchScoring ->
      "Search scoring: prefer complete, directly useful expressions for the goal type.\n"
        ++ "Useful variables and literals can be modest scores; useful compound expressions should score higher.\n"
    HoleExpansionScoring ->
      "Hole-expansion scoring: partial expressions with typed holes are allowed.\n"
        ++ "Prefer partial expressions only when they are good next expansion steps.\n"
        ++ "Complete useful replacements are also valid and should not be forced to 0.\n"
    FunctionBodyScoring ->
      "Function-body scoring: this candidate should be a complete Haskell function body.\n"
        ++ "Strongly prefer complete, directly useful expressions.\n"
        ++ "Expressions containing holes should receive a very low score.\n"
