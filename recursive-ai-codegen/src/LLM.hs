{-# LANGUAGE OverloadedStrings #-}

module LLM
  ( canUseLLMScoring
  , scoreWithLLM
  ) where

import Config
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , decode
  , eitherDecode
  , encode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (isDigit)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Network.HTTP.Simple
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

data ChatResponse = ChatResponse [Choice]

data Choice = Choice Message (Maybe T.Text)

data Message = Message T.Text (Maybe T.Text)

instance FromJSON ChatResponse where
  parseJSON =
    withObject "ChatResponse" $ \value ->
      ChatResponse <$> value .: "choices"

instance FromJSON Choice where
  parseJSON =
    withObject "Choice" $ \value ->
      Choice <$> value .: "message"
        <*> value .:? "finish_reason"

instance FromJSON Message where
  parseJSON =
    withObject "Message" $ \value -> do
      content <- value .:? "content"
      reasoningContent <- value .:? "reasoning_content"
      return (Message (maybe "" id content) reasoningContent)

scoreWithLLM :: String -> IO (Maybe Double)
scoreWithLLM prompt = do
  config <- readLLMConfig
  result <- try (requestLLMScore config prompt) :: IO (Either SomeException (Either String Double))
  case result of
    Left exception -> do
      warnLLMFailure (summarizeException (show exception))
      return Nothing
    Right (Left message) -> do
      warnLLMFailure message
      return Nothing
    Right (Right score) ->
      return (Just score)

canUseLLMScoring :: IO Bool
canUseLLMScoring = do
  maybeScore <- scoreWithLLM "Return only JSON matching this shape exactly: {\"score\":50}. Do not include markdown or explanation."
  case maybeScore of
    Nothing ->
      return False
    Just _ ->
      return True

requestLLMScore :: LLMConfig -> String -> IO (Either String Double)
requestLLMScore config prompt = do
  baseRequest <- parseRequest (llmBaseURL config)
  let request =
        setRequestMethod "POST"
          ( setRequestHeader "Content-Type" ["application/json"]
              (setRequestBodyLBS (requestBody config prompt) baseRequest)
          )
  response <- httpLBS request
  let statusCode = getResponseStatusCode response
      responseBody = getResponseBody response
  logDebugResponse responseBody
  if statusCode < 200 || statusCode >= 300
    then return (Left (httpErrorMessage statusCode responseBody))
    else return (decodeScore responseBody)

logDebugResponse :: BL.ByteString -> IO ()
logDebugResponse responseBody = do
  debug <- lookupEnv "LLM_DEBUG_RESPONSE"
  case debug of
    Just "1" ->
      hPutStrLn stderr ("LLM raw response: " ++ take 3000 (BL8.unpack responseBody))
    _ ->
      return ()

requestBody :: LLMConfig -> String -> BL.ByteString
requestBody config prompt =
  encode
    ( object
        [ "model" .= T.pack (llmModel config)
        , "messages" .=
            [ object
                [ "role" .= ("user" :: T.Text)
                , "content" .= T.pack prompt
                ]
            ]
        , "response_format" .= scoreResponseFormat
        , "temperature" .= (0.1 :: Double)
        , "max_tokens" .= llmMaxTokens config
        , "stream" .= False
        ]
    )

scoreResponseFormat :: Value
scoreResponseFormat =
  object
    [ "type" .= ("json_schema" :: T.Text)
    , "json_schema" .=
        object
          [ "name" .= ("candidate_score" :: T.Text)
          , "strict" .= True
          , "schema" .=
              object
                [ "type" .= ("object" :: T.Text)
                , "properties" .=
                    object
                      [ "score" .=
                          object
                            [ "type" .= ("number" :: T.Text)
                            , "minimum" .= (0 :: Int)
                            , "maximum" .= (100 :: Int)
                            ]
                      ]
                , "required" .= [ "score" :: T.Text ]
                , "additionalProperties" .= False
                ]
          ]
    ]

decodeScore :: BL.ByteString -> Either String Double
decodeScore responseBody =
  case eitherDecode responseBody of
    Left _ ->
      Left "could not decode JSON response"
    Right chatResponse ->
      scoreFromResponse chatResponse

scoreFromResponse :: ChatResponse -> Either String Double
scoreFromResponse (ChatResponse choices) =
  case choices of
    [] ->
      Left "response had no choices"
    Choice (Message content reasoningContent) finishReason : _ ->
      case parseStructuredScore content of
        Just score ->
          Right (clampScore score)
        Nothing ->
          case parseFirstNumber (T.unpack content) of
            Just score ->
              Right (clampScore score)
            Nothing ->
              case reasoningContent >>= parseStructuredScore of
                Just score ->
                  Right (clampScore score)
                Nothing ->
                  Left
                    ( "response did not contain a score"
                        ++ finishReasonText finishReason
                        ++ "; content was "
                        ++ show (take 160 (T.unpack content))
                        ++ reasoningContentText reasoningContent
                    )

finishReasonText :: Maybe T.Text -> String
finishReasonText finishReason =
  case finishReason of
    Nothing ->
      ""
    Just reason ->
      "; finish_reason=" ++ T.unpack reason

reasoningContentText :: Maybe T.Text -> String
reasoningContentText reasoningContent =
  case reasoningContent of
    Nothing ->
      ""
    Just text ->
      "; reasoning_content was " ++ show (take 160 (T.unpack text))

newtype ScoreResponse = ScoreResponse Double

instance FromJSON ScoreResponse where
  parseJSON =
    withObject "ScoreResponse" $ \value ->
      ScoreResponse <$> value .: "score"

parseStructuredScore :: T.Text -> Maybe Double
parseStructuredScore content =
  case decode (BL8.pack (T.unpack content)) of
    Nothing ->
      Nothing
    Just (ScoreResponse score) ->
      Just score

parseFirstNumber :: String -> Maybe Double
parseFirstNumber text =
  parseFrom (dropUntilNumber text)

parseFrom :: String -> Maybe Double
parseFrom text =
  case text of
    [] ->
      Nothing
    _ : rest ->
      case reads text of
        (number, _) : _ ->
          Just number
        [] ->
          parseFrom (dropUntilNumber rest)

dropUntilNumber :: String -> String
dropUntilNumber text =
  dropWhile (not . canStartNumber) text

canStartNumber :: Char -> Bool
canStartNumber char =
  isDigit char || char == '-' || char == '.'

clampScore :: Double -> Double
clampScore score
  | score < 0.0 = 0.0
  | score > 100.0 = 100.0
  | otherwise = score

warnLLMFailure :: String -> IO ()
warnLLMFailure message =
  hPutStrLn stderr ("Warning: LLM scoring failed; using fake score. " ++ message)

httpErrorMessage :: Int -> BL.ByteString -> String
httpErrorMessage statusCode responseBody =
  "HTTP status " ++ show statusCode ++ ": " ++ take 300 (BL8.unpack responseBody)

summarizeException :: String -> String
summarizeException text
  | "Connection refused" `isInfixOf` text =
      "connection refused at LLM endpoint"
  | "ResponseTimeout" `isInfixOf` text =
      "request timed out"
  | "InvalidUrlException" `isInfixOf` text =
      "invalid LLM_BASE_URL"
  | otherwise =
      "request failed"
