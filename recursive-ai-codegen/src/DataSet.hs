module DataSet
  ( Env
  , defaultEnv
  , defaultGoals
  , searchDepth
  , trainingLogPath
  ) where

import AST

type Env = [(String, Type)]

defaultEnv :: Env
defaultEnv =
  [ ("x", TInt)
  , ("y", TInt)
  , ("nums", TList TInt)
  ]

defaultGoals :: [Type]
defaultGoals =
  [ TInt
  , TBool
  ]

searchDepth :: Int
searchDepth =
  2

trainingLogPath :: FilePath
trainingLogPath =
  "data/training-log.jsonl"
