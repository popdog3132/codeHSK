module Task
  ( TaskSpec(..)
  , sumTask
  , lengthTask
  ) where

import AST
import DataSet

data TaskSpec = TaskSpec
  { taskName :: String
  , taskDescription :: String
  , taskGoalType :: Type
  , taskEnv :: Env
  } deriving (Show, Eq)

sumTask :: TaskSpec
sumTask =
  TaskSpec
    { taskName = "generatedSum"
    , taskDescription = "Return the sum of the input list nums."
    , taskGoalType = TInt
    , taskEnv = [("nums", TList TInt)]
    }

lengthTask :: TaskSpec
lengthTask =
  TaskSpec
    { taskName = "generatedLength"
    , taskDescription = "Return the length of the input list nums."
    , taskGoalType = TInt
    , taskEnv = [("nums", TList TInt)]
    }
