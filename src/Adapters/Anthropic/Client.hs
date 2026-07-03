{-# LANGUAGE OverloadedStrings #-}

{- | Cliente HTTP compartido para la API de Anthropic.

Extrae la maquinaria de red que antes estaba duplicada en
@Adapters.BehaviourResolver@ y @Adapters.LevelGenerator@: configuración del
manager, dispatch del status HTTP, parseo de la respuesta y helpers de log\/debug.

'callAnthropic' hace una única llamada POST y devuelve el primer bloque de texto
de la respuesta, o 'Nothing' en cualquier error (red, status no-2xx, JSON inesperado).

El llamador construye el @body@ completo (modelo, tokens, temperatura, mensajes);
'callAnthropic' solo inyecta los headers de autenticación y manejo de errores.
-}
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

-- Grupo 1 — stdlib / base
import Control.Exception (SomeException, try)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.IO (hPutStrLn, stderr)

import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text qualified as T

-- Grupo 2 — terceros
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

-- ---------------------------------------------------------------------------
-- Tipos públicos
-- ---------------------------------------------------------------------------

-- | Conexión reusable: manager TLS, API key y base URL.
data AnthropicClient = AnthropicClient
  { acManager :: Manager
  , acApiKey :: Text
  , acBaseUrl :: String
  }

{- | Parámetros por feature (modelo, timeout, nivel de debug).

Permite configurar el generador y el resolver con distintos modelos y timeouts
desde el mismo 'AnthropicClient'.
-}
data FeatureCfg = FeatureCfg
  { fcModel :: Text
  -- ^ ID de modelo de Anthropic (e.g. @claude-haiku-4-5@).
  , fcTimeoutMicros :: Int
  -- ^ Timeout por llamada en microsegundos.
  , fcDebug :: Bool
  -- ^ Habilita logs de debug en @stderr@.
  }

-- ---------------------------------------------------------------------------
-- Tipos de respuesta API (privados al módulo)
-- ---------------------------------------------------------------------------

newtype AnthropicApiResponse = AnthropicApiResponse {arContent :: [AnthropicBlock]}

-- | @text@ es opcional: bloques @thinking@\/@tool_use@ no rompen el decode.
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

-- ---------------------------------------------------------------------------
-- API pública
-- ---------------------------------------------------------------------------

{- | Hace una llamada POST a la API de Anthropic y devuelve el primer texto.

@body@ es el objeto JSON completo del request (debe incluir @model@, @max_tokens@,
@temperature@ y @messages@). Cualquier falla (red, status no-2xx, parse) →
'Nothing' + warning en @stderr@.
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

-- ---------------------------------------------------------------------------
-- Helpers exportados
-- ---------------------------------------------------------------------------

-- | Primer bloque de tipo @text@ en la respuesta de la API.
firstText :: AnthropicApiResponse -> Maybe Text
firstText resp = do
  block <- find ((== "text") . abType) (arContent resp)
  abText block

-- | Rango de estado HTTP exitoso.
inRange2xx :: Int -> Bool
inRange2xx code = code >= 200 && code < 300

-- | Filtra key vacía o solo espacios; evita 401 por hint.
nonEmptyApiKey :: Maybe String -> Maybe Text
nonEmptyApiKey raw = do
  s <- raw
  let trimmed = T.strip (T.pack s)
  if T.null trimmed then Nothing else Just trimmed

-- | Emite línea de debug en @stderr@ si @enabled@.
debugLog :: Bool -> String -> String -> IO ()
debugLog enabled tag msg
  | enabled = hPutStrLn stderr ("[" <> tag <> ":debug] " <> msg)
  | otherwise = pure ()

-- | Cantidad de caracteres de respuesta cruda que muestran los logs de debug.
previewLimit :: Int
previewLimit = 600

-- | Primeros 'previewLimit' caracteres de un texto para debug.
previewText :: Text -> String
previewText = T.unpack . T.take previewLimit

-- | Primeros 'previewLimit' caracteres del cuerpo de respuesta para debug.
previewBody :: BL.ByteString -> String
previewBody = previewText . decodeUtf8Lenient . BL.toStrict
