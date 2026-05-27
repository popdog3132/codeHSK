module Memory
  ( MemoryPattern(..)
  , memoryPatterns
  , patternsForGoal
  , patternCandidates
  ) where

import AST
import DataSet

data MemoryPattern = MemoryPattern
  { patternName :: String
  , patternType :: Type
  , patternExpr :: Expr
  , patternDescription :: String
  } deriving (Show, Eq)

memoryPatterns :: Env -> [MemoryPattern]
memoryPatterns env =
  intListPatterns env ++ intPatterns env

patternsForGoal :: Env -> Type -> [MemoryPattern]
patternsForGoal env goalType =
  [ memoryPattern
  | memoryPattern <- memoryPatterns env
  , patternType memoryPattern == goalType
  ]

patternCandidates :: Env -> Type -> [Expr]
patternCandidates env goalType =
  map patternExpr (patternsForGoal env goalType)

intPatterns :: Env -> [MemoryPattern]
intPatterns env =
  sumNumsPattern env
    ++ lengthNumsPattern env
    ++ addXYPattern env
    ++ mulXYPattern env
    ++ ifXYPattern env

intListPatterns :: Env -> [MemoryPattern]
intListPatterns env =
  numsPattern env
    ++ [ MemoryPattern
           { patternName = "empty-int-list"
           , patternType = TList TInt
           , patternExpr = List [] TInt
           , patternDescription = "The empty Int list."
           }
       , MemoryPattern
           { patternName = "small-int-list"
           , patternType = TList TInt
           , patternExpr = List [LitInt 1, LitInt 2, LitInt 3] TInt
           , patternDescription = "A small concrete Int list."
           }
       ]

sumNumsPattern :: Env -> [MemoryPattern]
sumNumsPattern env =
  if hasBinding "nums" (TList TInt) env
    then
      [ MemoryPattern
          { patternName = "sum-nums"
          , patternType = TInt
          , patternExpr = Sum numsExpr
          , patternDescription = "Sum all Int values in nums."
          }
      ]
    else []

lengthNumsPattern :: Env -> [MemoryPattern]
lengthNumsPattern env =
  if hasBinding "nums" (TList TInt) env
    then
      [ MemoryPattern
          { patternName = "length-nums"
          , patternType = TInt
          , patternExpr = Length numsExpr
          , patternDescription = "Count the Int values in nums."
          }
      ]
    else []

addXYPattern :: Env -> [MemoryPattern]
addXYPattern env =
  if hasXY env
    then
      [ MemoryPattern
          { patternName = "add-x-y"
          , patternType = TInt
          , patternExpr = Add xExpr yExpr
          , patternDescription = "Add x and y."
          }
      ]
    else []

mulXYPattern :: Env -> [MemoryPattern]
mulXYPattern env =
  if hasXY env
    then
      [ MemoryPattern
          { patternName = "mul-x-y"
          , patternType = TInt
          , patternExpr = Mul xExpr yExpr
          , patternDescription = "Multiply x and y."
          }
      ]
    else []

ifXYPattern :: Env -> [MemoryPattern]
ifXYPattern env =
  if hasXY env
    then
      [ MemoryPattern
          { patternName = "if-x-eq-y"
          , patternType = TInt
          , patternExpr = If (Eq xExpr yExpr) xExpr yExpr
          , patternDescription = "Return x when x equals y, otherwise y."
          }
      ]
    else []

numsPattern :: Env -> [MemoryPattern]
numsPattern env =
  if hasBinding "nums" (TList TInt) env
    then
      [ MemoryPattern
          { patternName = "nums-list"
          , patternType = TList TInt
          , patternExpr = numsExpr
          , patternDescription = "Use the nums argument directly."
          }
      ]
    else []

hasXY :: Env -> Bool
hasXY env =
  hasBinding "x" TInt env && hasBinding "y" TInt env

hasBinding :: String -> Type -> Env -> Bool
hasBinding wantedName wantedType env =
  any matches env
  where
    matches :: (String, Type) -> Bool
    matches (name, typeValue) =
      name == wantedName && typeValue == wantedType

numsExpr :: Expr
numsExpr =
  Var "nums" (TList TInt)

xExpr :: Expr
xExpr =
  Var "x" TInt

yExpr :: Expr
yExpr =
  Var "y" TInt
