module Pretty
  ( prettyType
  , prettyExpr
  ) where

import AST
import Data.List (intercalate)

prettyType :: Type -> String
prettyType typeValue =
  case typeValue of
    TInt ->
      "Int"
    TBool ->
      "Bool"
    TList itemType ->
      "[" ++ prettyType itemType ++ "]"
    TFun inputType outputType ->
      prettyFunctionInput inputType ++ " -> " ++ prettyType outputType

prettyExpr :: Expr -> String
prettyExpr expr =
  case expr of
    Var name _ ->
      name
    Hole holeId holeType ->
      "<hole" ++ show holeId ++ ":" ++ prettyType holeType ++ ">"
    LitInt number ->
      show number
    LitBool boolValue ->
      if boolValue then "True" else "False"
    Add left right ->
      "(" ++ prettyExpr left ++ " + " ++ prettyExpr right ++ ")"
    Mul left right ->
      "(" ++ prettyExpr left ++ " * " ++ prettyExpr right ++ ")"
    Eq left right ->
      "(" ++ prettyExpr left ++ " == " ++ prettyExpr right ++ ")"
    If condition trueBranch falseBranch ->
      "if " ++ prettyExpr condition
        ++ " then " ++ prettyExpr trueBranch
        ++ " else " ++ prettyExpr falseBranch
    List items _ ->
      "[" ++ intercalate ", " (map prettyExpr items) ++ "]"
    Length listExpr ->
      "length " ++ prettyAtom listExpr
    Sum listExpr ->
      "sum " ++ prettyAtom listExpr

prettyFunctionInput :: Type -> String
prettyFunctionInput typeValue =
  case typeValue of
    TFun _ _ ->
      "(" ++ prettyType typeValue ++ ")"
    _ ->
      prettyType typeValue

prettyAtom :: Expr -> String
prettyAtom expr =
  case expr of
    Var _ _ ->
      prettyExpr expr
    Hole _ _ ->
      prettyExpr expr
    LitInt _ ->
      prettyExpr expr
    LitBool _ ->
      prettyExpr expr
    List _ _ ->
      prettyExpr expr
    _ ->
      "(" ++ prettyExpr expr ++ ")"
