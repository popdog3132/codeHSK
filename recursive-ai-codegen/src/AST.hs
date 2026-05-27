module AST
  ( Type(..)
  , Expr(..)
  , exprType
  , exprSize
  ) where

data Type
  = TInt
  | TBool
  | TList Type
  | TFun Type Type
  deriving (Show, Eq)

data Expr
  = Var String Type
  | Hole Int Type
  | LitInt Int
  | LitBool Bool
  | Add Expr Expr
  | Mul Expr Expr
  | Eq Expr Expr
  | If Expr Expr Expr
  | List [Expr] Type
  | Length Expr
  | Sum Expr
  deriving (Show, Eq)

exprType :: Expr -> Type
exprType expr =
  case expr of
    Var _ varType ->
      varType
    Hole _ holeType ->
      holeType
    LitInt _ ->
      TInt
    LitBool _ ->
      TBool
    Add _ _ ->
      TInt
    Mul _ _ ->
      TInt
    Eq _ _ ->
      TBool
    If _ trueBranch _ ->
      exprType trueBranch
    List _ itemType ->
      TList itemType
    Length _ ->
      TInt
    Sum _ ->
      TInt

exprSize :: Expr -> Int
exprSize expr =
  case expr of
    Var _ _ ->
      1
    Hole _ _ ->
      1
    LitInt _ ->
      1
    LitBool _ ->
      1
    Add left right ->
      1 + exprSize left + exprSize right
    Mul left right ->
      1 + exprSize left + exprSize right
    Eq left right ->
      1 + exprSize left + exprSize right
    If condition trueBranch falseBranch ->
      1 + exprSize condition + exprSize trueBranch + exprSize falseBranch
    List items _ ->
      1 + sum (map exprSize items)
    Length listExpr ->
      1 + exprSize listExpr
    Sum listExpr ->
      1 + exprSize listExpr
