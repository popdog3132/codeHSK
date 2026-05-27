module TestRunner
  ( TestResult(..)
  , runFunctionTests
  ) where

import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

data TestResult = TestResult
  { testPassed :: Bool
  , testExitCode :: String
  , testStdout :: String
  , testStderr :: String
  } deriving (Show, Eq)

runFunctionTests :: FilePath -> String -> IO TestResult
runFunctionTests path source = do
  writeFile path source
  result <- try (readProcessWithExitCode "runghc" [path] "") :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Left exception ->
      return
        TestResult
          { testPassed = False
          , testExitCode = "exception"
          , testStdout = ""
          , testStderr = show exception
          }
    Right (exitCode, stdoutText, stderrText) ->
      return
        TestResult
          { testPassed = exitCode == ExitSuccess && "PASS" `isInfixOf` stdoutText
          , testExitCode = show exitCode
          , testStdout = stdoutText
          , testStderr = stderrText
          }
