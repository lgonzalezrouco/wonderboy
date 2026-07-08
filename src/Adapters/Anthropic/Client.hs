{-# LANGUAGE OverloadedStrings #-}

module Adapters.Anthropic.Client (
  AnthropicClient (..),
  FeatureCfg (..),
  callAnthropic,
  inRange2xx,
  nonEmptyApiKey,
  debugLog,
  previewBody,
  previewText,
)
where

import Control.Exception (SomeException, try)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.IO (hPutStrLn, stderr)

import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text qualified as T

import Data.Aeson (
  FromJSON (..),
  Value,
  encode,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson qualified as Aeson
import Network.HTTP.Client (
  Manager,
  RequestBody (RequestBodyLBS),
  Response (responseBody, responseStatus),
  httpLbs,
  method,
  parseRequest,
  redactHeaders,
  requestBody,
  requestHeaders,
  responseTimeout,
  responseTimeoutMicro,
 )
import Network.HTTP.Types.Status (statusCode)

data AnthropicClient = AnthropicClient
  { acManager :: Manager
  , acApiKey :: Text
  , acBaseUrl :: String
  }

data FeatureCfg = FeatureCfg
  { fcModel :: Text
  , fcTimeoutMicros :: Int
  , fcDebug :: Bool
  }

newtype AnthropicApiResponse = AnthropicApiResponse {arContent :: [AnthropicBlock]}

-- | @abText@ es opcional para que los bloques @thinking@ / @tool_use@ (que no traen texto) no rompan el decoding.
data AnthropicBlock = AnthropicBlock
  { abType :: Text
  , abText :: Maybe Text
  }

instance FromJSON AnthropicApiResponse where
  parseJSON =
    withObject "AnthropicApiResponse" $ \o ->
      AnthropicApiResponse <$> o .: "content"

instance FromJSON AnthropicBlock where
  parseJSON =
    withObject "AnthropicBlock" $ \o ->
      AnthropicBlock <$> o .: "type" <*> o .:? "text"

{- | Un único POST a la API, que devuelve el primer bloque de texto, o 'Nothing' ante cualquier
falla (error de red, status distinto de 2xx, o JSON inesperado).
-}
callAnthropic :: AnthropicClient -> FeatureCfg -> Value -> IO (Maybe Text)
callAnthropic client cfg body = do
  result <- try @SomeException $ do
    baseReq <- parseRequest (acBaseUrl client)
    let req =
          baseReq
            { method = "POST"
            , requestHeaders =
                [ ("x-api-key", encodeUtf8 (acApiKey client))
                , ("anthropic-version", "2023-06-01")
                , ("content-type", "application/json")
                ]
            , requestBody = RequestBodyLBS (encode body)
            , responseTimeout = responseTimeoutMicro (fcTimeoutMicros cfg)
            , redactHeaders = Set.fromList ["x-api-key"]
            }
    httpLbs req (acManager client)
  case result of
    Left err -> warn ("falla de red: " <> show err)
    Right resp
      | inRange2xx (statusCode (responseStatus resp)) -> do
          debugLog (fcDebug cfg) "anthropic-client" $
            "status "
              <> show (statusCode (responseStatus resp))
              <> "; cuerpo crudo: "
              <> previewBody (responseBody resp)
          case Aeson.decode @AnthropicApiResponse (responseBody resp) of
            Nothing -> warn "JSON de respuesta inesperado"
            Just apiResp -> case firstText apiResp of
              Nothing -> warn "respuesta sin bloque de texto"
              Just txt -> pure (Just txt)
      | otherwise ->
          warn ("status inesperado: " <> show (statusCode (responseStatus resp)))
 where
  warn msg = hPutStrLn stderr ("[anthropic-client] " <> msg) >> pure Nothing

firstText :: AnthropicApiResponse -> Maybe Text
firstText resp = do
  block <- find ((== "text") . abType) (arContent resp)
  abText block

inRange2xx :: Int -> Bool
inRange2xx code = code >= 200 && code < 300

nonEmptyApiKey :: Maybe String -> Maybe Text
nonEmptyApiKey raw = do
  s <- raw
  let trimmed = T.strip (T.pack s)
  if T.null trimmed then Nothing else Just trimmed

debugLog :: Bool -> String -> String -> IO ()
debugLog enabled tag msg
  | enabled = hPutStrLn stderr ("[" <> tag <> ":debug] " <> msg)
  | otherwise = pure ()

previewLimit :: Int
previewLimit = 600

previewText :: Text -> String
previewText = T.unpack . T.take previewLimit

previewBody :: BL.ByteString -> String
previewBody = previewText . decodeUtf8Lenient . BL.toStrict
