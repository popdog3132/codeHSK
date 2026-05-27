module Holes
  ( ExpansionDecision(..)
  , ReplacementCandidate(..)
  , hasHoles
  , holesIn
  , nextHoleId
  , replaceHole
  , initialHole
  , holeExpansions
  , expandStepFake
  , expandStepLLM
  , expandFullyFake
  , expandFullyLLM
  , expandStepFakeDecision
  , expandStepLLMDecision
  , expandStepLLMDecisionWith
  ) where

import AST
import DataSet
import LLM
import Memory
import Score
import Data.List (sortBy)

data ReplacementCandidate = ReplacementCandidate
  { replacementExpr :: Expr
  , replacementOrigin :: String
  , replacementPatternName :: String
  } deriving (Show, Eq)

data ExpansionDecision = ExpansionDecision
  { expansionOriginal :: Expr
  , expansionHoleId :: Int
  , expansionHoleType :: Type
  , expansionCandidates :: [(ReplacementCandidate, ScoredExpr)]
  , expansionChosenReplacement :: Expr
  , expansionChosenOrigin :: String
  , expansionChosenPatternName :: String
  , expansionMemoryPatternNames :: [String]
  , expansionNewExpr :: Expr
  , expansionChosenScore :: ScoredExpr
  } deriving (Show, Eq)

hasHoles :: Expr -> Bool
hasHoles expr =
  not (null (holesIn expr))

holesIn :: Expr -> [(Int, Type)]
holesIn expr =
  case expr of
    Var _ _ ->
      []
    Hole holeId holeType ->
      [(holeId, holeType)]
    LitInt _ ->
      []
    LitBool _ ->
      []
    Add left right ->
      holesIn left ++ holesIn right
    Mul left right ->
      holesIn left ++ holesIn right
    Eq left right ->
      holesIn left ++ holesIn right
    If condition trueBranch falseBranch ->
      holesIn condition ++ holesIn trueBranch ++ holesIn falseBranch
    List items _ ->
      concatMap holesIn items
    Length listExpr ->
      holesIn listExpr
    Sum listExpr ->
      holesIn listExpr

nextHoleId :: Expr -> Int
nextHoleId expr =
  case holesIn expr of
    [] ->
      0
    holes ->
      1 + maximum (map fst holes)

replaceHole :: Int -> Expr -> Expr -> Expr
replaceHole targetHoleId replacement expr =
  case expr of
    Var _ _ ->
      expr
    Hole holeId _ ->
      if holeId == targetHoleId then replacement else expr
    LitInt _ ->
      expr
    LitBool _ ->
      expr
    Add left right ->
      Add (replaceHole targetHoleId replacement left) (replaceHole targetHoleId replacement right)
    Mul left right ->
      Mul (replaceHole targetHoleId replacement left) (replaceHole targetHoleId replacement right)
    Eq left right ->
      Eq (replaceHole targetHoleId replacement left) (replaceHole targetHoleId replacement right)
    If condition trueBranch falseBranch ->
      If
        (replaceHole targetHoleId replacement condition)
        (replaceHole targetHoleId replacement trueBranch)
        (replaceHole targetHoleId replacement falseBranch)
    List items itemType ->
      List (map (replaceHole targetHoleId replacement) items) itemType
    Length listExpr ->
      Length (replaceHole targetHoleId replacement listExpr)
    Sum listExpr ->
      Sum (replaceHole targetHoleId replacement listExpr)

initialHole :: Type -> Expr
initialHole goalType =
  Hole 0 goalType

holeExpansions :: Env -> Int -> Type -> [Expr]
holeExpansions env firstFreshHoleId holeType =
  map replacementExpr (holeExpansionCandidates env firstFreshHoleId holeType)

holeExpansionCandidates :: Env -> Int -> Type -> [ReplacementCandidate]
holeExpansionCandidates env firstFreshHoleId holeType =
  map generatedCandidate generatedReplacements ++ map memoryCandidate memoryReplacements
  where
    generatedReplacements :: [Expr]
    generatedReplacements =
      generatedHoleExpansions env firstFreshHoleId holeType

    memoryReplacements :: [MemoryPattern]
    memoryReplacements =
      patternsForGoal env holeType

generatedHoleExpansions :: Env -> Int -> Type -> [Expr]
generatedHoleExpansions env firstFreshHoleId holeType =
  case holeType of
    TInt ->
      variablesOfType env TInt
        ++ [ LitInt 0
           , LitInt 1
           , Add intHole1 intHole2
           , Mul intHole1 intHole2
           , Length intListHole1
           , Sum intListHole1
           , If boolHole1 intHole1 intHole2
           ]
    TBool ->
      [LitBool True, LitBool False]
        ++ variablesOfType env TBool
        ++ [Eq intHole1 intHole2]
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
  where
    intHole1 :: Expr
    intHole1 =
      Hole firstFreshHoleId TInt

    intHole2 :: Expr
    intHole2 =
      Hole (firstFreshHoleId + 1) TInt

    boolHole1 :: Expr
    boolHole1 =
      Hole (firstFreshHoleId + 2) TBool

    intListHole1 :: Expr
    intListHole1 =
      Hole (firstFreshHoleId + 3) (TList TInt)

generatedCandidate :: Expr -> ReplacementCandidate
generatedCandidate expr =
  ReplacementCandidate
    { replacementExpr = expr
    , replacementOrigin = "generated"
    , replacementPatternName = ""
    }

memoryCandidate :: MemoryPattern -> ReplacementCandidate
memoryCandidate memoryPattern =
  ReplacementCandidate
    { replacementExpr = patternExpr memoryPattern
    , replacementOrigin = "memory"
    , replacementPatternName = patternName memoryPattern
    }

expandStepFake :: Env -> Expr -> Maybe Expr
expandStepFake env expr =
  fmap expansionNewExpr (expandStepFakeDecision env expr)

expandStepLLM :: Env -> Expr -> IO (Maybe Expr)
expandStepLLM env expr = do
  maybeDecision <- expandStepLLMDecision env expr
  return (fmap expansionNewExpr maybeDecision)

expandFullyFake :: Env -> Int -> Type -> Expr
expandFullyFake env maxSteps goalType =
  expandFullyWithStep (expandStepFake env) maxSteps (initialHole goalType)

expandFullyLLM :: Env -> Int -> Type -> IO Expr
expandFullyLLM env maxSteps goalType = do
  canUseLLM <- canUseLLMScoring
  expandFullyLLMWith canUseLLM maxSteps (initialHole goalType)
  where
    expandFullyLLMWith :: Bool -> Int -> Expr -> IO Expr
    expandFullyLLMWith canUseLLM steps expr =
      if steps <= 0 || not (hasHoles expr)
        then return expr
        else do
          maybeDecision <- expandStepLLMDecisionWith canUseLLM env expr
          case maybeDecision of
            Nothing ->
              return expr
            Just decision ->
              expandFullyLLMWith canUseLLM (steps - 1) (expansionNewExpr decision)

expandStepFakeDecision :: Env -> Expr -> Maybe ExpansionDecision
expandStepFakeDecision env expr =
  buildExpansionDecision env expr (scoreExprFake goalType)
  where
    goalType :: Type
    goalType =
      exprType expr

expandStepLLMDecision :: Env -> Expr -> IO (Maybe ExpansionDecision)
expandStepLLMDecision env expr = do
  canUseLLM <- canUseLLMScoring
  expandStepLLMDecisionWith canUseLLM env expr

expandStepLLMDecisionWith :: Bool -> Env -> Expr -> IO (Maybe ExpansionDecision)
expandStepLLMDecisionWith canUseLLM env expr =
  buildExpansionDecisionIO env expr scoreCandidate
  where
    goalType :: Type
    goalType =
      exprType expr

    scoreCandidate :: Expr -> IO ScoredExpr
    scoreCandidate candidate =
      if canUseLLM
        then scoreExprLLM env goalType candidate
        else return (scoreExprFallback goalType candidate)

buildExpansionDecision :: Env -> Expr -> (Expr -> ScoredExpr) -> Maybe ExpansionDecision
buildExpansionDecision env expr scoreCandidate =
  case firstHole expr of
    Nothing ->
      Nothing
    Just (holeId, holeType) ->
      chooseExpansion expr holeId holeType scoredCandidates
      where
        scoredCandidates :: [(ReplacementCandidate, ScoredExpr)]
        scoredCandidates =
          map scoreReplacement (holeExpansionCandidates env (nextHoleId expr) holeType)

        scoreReplacement :: ReplacementCandidate -> (ReplacementCandidate, ScoredExpr)
        scoreReplacement candidate =
          (candidate, scoreCandidate (replaceHole holeId (replacementExpr candidate) expr))

buildExpansionDecisionIO :: Env -> Expr -> (Expr -> IO ScoredExpr) -> IO (Maybe ExpansionDecision)
buildExpansionDecisionIO env expr scoreCandidate =
  case firstHole expr of
    Nothing ->
      return Nothing
    Just (holeId, holeType) -> do
      scoredCandidates <- mapM scoreReplacement replacements
      return (chooseExpansion expr holeId holeType scoredCandidates)
      where
        replacements :: [ReplacementCandidate]
        replacements =
          holeExpansionCandidates env (nextHoleId expr) holeType

        scoreReplacement :: ReplacementCandidate -> IO (ReplacementCandidate, ScoredExpr)
        scoreReplacement candidate = do
          scored <- scoreCandidate (replaceHole holeId (replacementExpr candidate) expr)
          return (candidate, scored)

chooseExpansion :: Expr -> Int -> Type -> [(ReplacementCandidate, ScoredExpr)] -> Maybe ExpansionDecision
chooseExpansion original holeId holeType scoredCandidates =
  case sortBy compareReplacementScore scoredCandidates of
    [] ->
      Nothing
    (candidate, scored) : _ ->
      Just
        ExpansionDecision
          { expansionOriginal = original
          , expansionHoleId = holeId
          , expansionHoleType = holeType
          , expansionCandidates = scoredCandidates
          , expansionChosenReplacement = replacementExpr candidate
          , expansionChosenOrigin = replacementOrigin candidate
          , expansionChosenPatternName = replacementPatternName candidate
          , expansionMemoryPatternNames = memoryPatternNames scoredCandidates
          , expansionNewExpr = scoredExpr scored
          , expansionChosenScore = scored
          }

compareReplacementScore :: (ReplacementCandidate, ScoredExpr) -> (ReplacementCandidate, ScoredExpr) -> Ordering
compareReplacementScore (_, leftScored) (_, rightScored) =
  case compare (scoredScore rightScored) (scoredScore leftScored) of
    EQ ->
      compare (expansionRank (scoredExpr rightScored)) (expansionRank (scoredExpr leftScored))
    ordering ->
      ordering

expansionRank :: Expr -> Int
expansionRank expr =
  if hasHoles expr then 1 else 0

memoryPatternNames :: [(ReplacementCandidate, ScoredExpr)] -> [String]
memoryPatternNames scoredCandidates =
  uniqueStrings
    [ replacementPatternName candidate
    | (candidate, _) <- scoredCandidates
    , replacementOrigin candidate == "memory"
    ]

uniqueStrings :: [String] -> [String]
uniqueStrings values =
  case values of
    [] ->
      []
    value : rest ->
      value : uniqueStrings (filter (/= value) rest)

firstHole :: Expr -> Maybe (Int, Type)
firstHole expr =
  case holesIn expr of
    [] ->
      Nothing
    first : _ ->
      Just first

expandFullyWithStep :: (Expr -> Maybe Expr) -> Int -> Expr -> Expr
expandFullyWithStep step steps expr =
  if steps <= 0 || not (hasHoles expr)
    then expr
    else
      case step expr of
        Nothing ->
          expr
        Just nextExpr ->
          expandFullyWithStep step (steps - 1) nextExpr

variablesOfType :: Env -> Type -> [Expr]
variablesOfType env wantedType =
  [ Var name varType
  | (name, varType) <- env
  , varType == wantedType
  ]
