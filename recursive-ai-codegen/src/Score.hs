module Score
  ( ScoredExpr(..)
  , scoreExpr
  , scoreExprFake
  , scoreExprFallback
  , scoreExprLLM
  ) where

import AST
import DataSet
import LLM
import Prompt

data ScoredExpr = ScoredExpr
  { scoredExpr :: Expr
  , scoredScore :: Double
  , scoredReason :: String
  , scoredSource :: String
  } deriving (Show, Eq)

scoreExpr :: Type -> Expr -> ScoredExpr
scoreExpr =
  scoreExprFake

scoreExprFake :: Type -> Expr -> ScoredExpr
scoreExprFake goalType expr =
  ScoredExpr
    { scoredExpr = expr
    , scoredScore = totalScore
    , scoredReason = reason
    , scoredSource = "fake"
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
      incompleteFormBonus expr

    totalScore :: Double
    totalScore =
      typeScore + sizeBonus + usefulBonus + incompleteBonus

    typeReason :: String
    typeReason =
      if matchesGoal then "type matches goal" else "wrong type"

    reason :: String
    reason =
      typeReason
        ++ ", size bonus " ++ show sizeBonus
        ++ ", useful form bonus " ++ show usefulBonus
        ++ ", incomplete expression bonus " ++ show incompleteBonus

scoreExprFallback :: Type -> Expr -> ScoredExpr
scoreExprFallback goalType expr =
  fakeScore
    { scoredReason = scoredReason fakeScore ++ ", LLM unavailable"
    , scoredSource = "fallback"
    }
  where
    fakeScore :: ScoredExpr
    fakeScore =
      scoreExprFake goalType expr

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

incompleteFormBonus :: Expr -> Double
incompleteFormBonus expr =
  if exprHasHole expr
    then
      case expr of
        Add _ _ ->
          40.0
        If _ _ _ ->
          34.0
        Mul _ _ ->
          32.0
        Eq _ _ ->
          28.0
        Sum _ ->
          20.0
        Length _ ->
          20.0
        _ ->
          0.0
    else 0.0

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
scoreExprLLM env goalType expr =
  if exprType expr /= goalType
    then return wrongTypeScore
    else do
      maybeScore <- scoreWithLLM (candidatePrompt env goalType expr)
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
              }
  where
    fakeScore :: ScoredExpr
    fakeScore =
      scoreExprFake goalType expr

    fallbackScore :: ScoredExpr
    fallbackScore =
      scoreExprFallback goalType expr

    wrongTypeScore :: ScoredExpr
    wrongTypeScore =
      fakeScore
        { scoredScore = -1000.0
        , scoredReason = "wrong type, skipped LLM"
        , scoredSource = "fallback"
        }
