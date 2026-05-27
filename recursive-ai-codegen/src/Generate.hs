module Generate
  ( Env
  , SearchResult(..)
  , search
  , searchIO
  , generateBest
  , allCandidates
  ) where

import AST
import DataSet
import LLM
import Score
import Data.List (sortBy)

data SearchResult = SearchResult
  { searchCandidates :: [ScoredExpr]
  , searchBest :: Maybe ScoredExpr
  } deriving (Show, Eq)

search :: Env -> Int -> Type -> SearchResult
search env depth goalType =
  SearchResult
    { searchCandidates = sortedCandidates
    , searchBest = bestCandidate
    }
  where
    candidates :: [Expr]
    candidates =
      allCandidates env depth goalType

    sortedCandidates :: [ScoredExpr]
    sortedCandidates =
      sortBy compareScoredExpr (map (scoreExprFake goalType) candidates)

    bestCandidate :: Maybe ScoredExpr
    bestCandidate =
      case sortedCandidates of
        [] ->
          Nothing
        best : _ ->
          Just best

searchIO :: Env -> Int -> Type -> IO SearchResult
searchIO env depth goalType = do
  let candidates = allCandidates env depth goalType
  canUseLLM <- canUseLLMScoring
  scoredCandidates <-
    if canUseLLM
      then mapM (scoreExprLLM env goalType) candidates
      else return (map (scoreExprFallback goalType) candidates)
  let sortedCandidates = sortBy compareScoredExpr scoredCandidates
  return
    SearchResult
      { searchCandidates = sortedCandidates
      , searchBest = bestScoredExpr sortedCandidates
      }

generateBest :: Env -> Int -> Type -> Maybe Expr
generateBest env depth goalType =
  case searchBest (search env depth goalType) of
    Nothing ->
      Nothing
    Just scored ->
      Just (scoredExpr scored)

allCandidates :: Env -> Int -> Type -> [Expr]
allCandidates env depth goalType =
  terminals ++ recursiveCandidates
  where
    terminals :: [Expr]
    terminals =
      terminalCandidates env goalType

    recursiveCandidates :: [Expr]
    recursiveCandidates =
      if depth <= 0
        then []
        else compoundCandidates env (depth - 1) goalType

terminalCandidates :: Env -> Type -> [Expr]
terminalCandidates env goalType =
  case goalType of
    TInt ->
      variablesOfType env TInt ++ [LitInt 0, LitInt 1]
    TBool ->
      variablesOfType env TBool ++ [LitBool True, LitBool False]
    TList TInt ->
      variablesOfType env (TList TInt)
        ++ [ List [] TInt
           , List [LitInt 1] TInt
           , List [LitInt 1, LitInt 2, LitInt 3] TInt
           ]
    TList itemType ->
      variablesOfType env (TList itemType)
    TFun inputType outputType ->
      variablesOfType env (TFun inputType outputType)

compoundCandidates :: Env -> Int -> Type -> [Expr]
compoundCandidates env smallerDepth goalType =
  case goalType of
    TInt ->
      intCompoundCandidates env smallerDepth
    TBool ->
      boolCompoundCandidates env smallerDepth
    TList _ ->
      []
    TFun _ _ ->
      []

intCompoundCandidates :: Env -> Int -> [Expr]
intCompoundCandidates env smallerDepth =
  addCandidates
    ++ mulCandidates
    ++ lengthCandidates
    ++ sumCandidates
    ++ ifCandidates
  where
    intCandidates :: [Expr]
    intCandidates =
      operandCandidates env smallerDepth TInt

    boolCandidates :: [Expr]
    boolCandidates =
      operandCandidates env smallerDepth TBool

    intListCandidates :: [Expr]
    intListCandidates =
      operandCandidates env smallerDepth (TList TInt)

    addCandidates :: [Expr]
    addCandidates =
      combineTwo Add intCandidates intCandidates

    mulCandidates :: [Expr]
    mulCandidates =
      combineTwo Mul intCandidates intCandidates

    lengthCandidates :: [Expr]
    lengthCandidates =
      map Length intListCandidates

    sumCandidates :: [Expr]
    sumCandidates =
      map Sum intListCandidates

    ifCandidates :: [Expr]
    ifCandidates =
      [ If condition trueBranch falseBranch
      | condition <- boolCandidates
      , trueBranch <- intCandidates
      , falseBranch <- intCandidates
      ]

boolCompoundCandidates :: Env -> Int -> [Expr]
boolCompoundCandidates env smallerDepth =
  combineTwo Eq intCandidates intCandidates
  where
    intCandidates :: [Expr]
    intCandidates =
      operandCandidates env smallerDepth TInt

variablesOfType :: Env -> Type -> [Expr]
variablesOfType env wantedType =
  [ Var name varType
  | (name, varType) <- env
  , varType == wantedType
  ]

combineTwo :: (Expr -> Expr -> Expr) -> [Expr] -> [Expr] -> [Expr]
combineTwo makeExpr leftValues rightValues =
  [ makeExpr left right
  | left <- leftValues
  , right <- rightValues
  ]

compareScoredExpr :: ScoredExpr -> ScoredExpr -> Ordering
compareScoredExpr left right =
  compare (scoredScore right) (scoredScore left)

bestScoredExpr :: [ScoredExpr] -> Maybe ScoredExpr
bestScoredExpr scoredCandidates =
  case scoredCandidates of
    [] ->
      Nothing
    best : _ ->
      Just best

operandCandidates :: Env -> Int -> Type -> [Expr]
operandCandidates env depth goalType =
  take 4 (allCandidates env depth goalType)
