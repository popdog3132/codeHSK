module Prompt
  ( candidatePrompt
  ) where

import AST
import DataSet
import Pretty

candidatePrompt :: Env -> Type -> Expr -> String
candidatePrompt env goalType expr =
  "Goal type: " ++ prettyType goalType ++ "\n"
    ++ "Environment:\n"
    ++ concatMap envLine env
    ++ "\n"
    ++ "Candidate:\n"
    ++ prettyExpr expr ++ "\n\n"
    ++ "Score this candidate from 0 to 100.\n"
    ++ "Higher means more likely useful, correct, and relevant.\n"
    ++ "Return JSON with one field named score."

envLine :: (String, Type) -> String
envLine (name, typeValue) =
  name ++ " :: " ++ prettyType typeValue ++ "\n"
