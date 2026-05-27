module Config
  ( LLMConfig(..)
  , readLLMConfig
  ) where

import System.Environment (lookupEnv)

data LLMConfig = LLMConfig
  { llmBaseURL :: String
  , llmModel :: String
  , llmMaxTokens :: Int
  } deriving (Show, Eq)

readLLMConfig :: IO LLMConfig
readLLMConfig = do
  baseURL <- readEnvWithDefault "LLM_BASE_URL" defaultBaseURL
  model <- readEnvWithDefault "LLM_MODEL" defaultModel
  maxTokens <- readIntEnvWithDefault "LLM_MAX_TOKENS" defaultMaxTokens
  return
    LLMConfig
      { llmBaseURL = baseURL
      , llmModel = model
      , llmMaxTokens = maxTokens
      }

readEnvWithDefault :: String -> String -> IO String
readEnvWithDefault name defaultValue = do
  maybeValue <- lookupEnv name
  case maybeValue of
    Nothing ->
      return defaultValue
    Just "" ->
      return defaultValue
    Just value ->
      return value

readIntEnvWithDefault :: String -> Int -> IO Int
readIntEnvWithDefault name defaultValue = do
  value <- readEnvWithDefault name (show defaultValue)
  case reads value of
    (number, "") : _ ->
      return number
    _ ->
      return defaultValue

defaultBaseURL :: String
defaultBaseURL =
  "http://127.0.0.1:1234/v1/chat/completions"

defaultModel :: String
defaultModel =
  "openai/gpt-oss-20b"

defaultMaxTokens :: Int
defaultMaxTokens =
  256
