module FunctionGen
  ( FunctionSpec(..)
  , TestCase(..)
  , prettyFunction
  , functionSource
  , functionSourceWithTests
  , functionTypeText
  ) where

import AST
import DataSet
import Pretty
import Data.List (intercalate)

data FunctionSpec = FunctionSpec
  { functionName :: String
  , functionArgs :: Env
  , functionReturnType :: Type
  , functionBody :: Expr
  } deriving (Show, Eq)

data TestCase = TestCase
  { testCall :: String
  , expectedValue :: String
  } deriving (Show, Eq)

prettyFunction :: FunctionSpec -> String
prettyFunction spec =
  functionName spec ++ " :: " ++ functionTypeText spec ++ "\n"
    ++ functionName spec ++ argumentText ++ " = " ++ prettyExpr (functionBody spec)
  where
    argumentText :: String
    argumentText =
      case map fst (functionArgs spec) of
        [] ->
          ""
        names ->
          " " ++ unwords names

functionTypeText :: FunctionSpec -> String
functionTypeText spec =
  intercalate " -> " (argumentTypes ++ [prettyType (functionReturnType spec)])
  where
    argumentTypes :: [String]
    argumentTypes =
      map (prettyType . snd) (functionArgs spec)

functionSource :: FunctionSpec -> String
functionSource spec =
  "module Main where\n\n"
    ++ prettyFunction spec
    ++ "\n\n"
    ++ "main :: IO ()\n"
    ++ "main = return ()\n"

functionSourceWithTests :: FunctionSpec -> [TestCase] -> String
functionSourceWithTests spec tests =
  "module Main where\n\n"
    ++ prettyFunction spec
    ++ "\n\n"
    ++ testMain tests

testMain :: [TestCase] -> String
testMain tests =
  "main :: IO ()\n"
    ++ "main =\n"
    ++ "  if " ++ testCondition tests ++ "\n"
    ++ "    then putStrLn \"PASS\"\n"
    ++ "    else putStrLn \"FAIL\"\n"

testCondition :: [TestCase] -> String
testCondition tests =
  case tests of
    [] ->
      "and []"
    _ ->
      "and\n       [ "
        ++ intercalate "\n       , " (map testExpression tests)
        ++ "\n       ]"

testExpression :: TestCase -> String
testExpression test =
  testCall test ++ " == " ++ expectedValue test
