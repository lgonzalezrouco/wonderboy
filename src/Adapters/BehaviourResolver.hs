{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Adaptador de 'BehaviourResolverPort': resuelve @behaviourHint@ vía la API de
Anthropic. Todo el 'IO' del feature vive acá.

'AnthropicResolver' (@ReaderT ResolverEnv IO@) evita una instancia huérfana sobre 'IO'
y mantiene @UseCases/@ libre de este módulo. Cualquier falla degrada a 'Nothing' y el
build cae al default del kind.
-}
module Adapters.BehaviourResolver (
  resolveLevelIO,
  ResolverReply (..),
  resolvedFromReply,
  extractJsonObject,
)
where

-- Grupo 1 — stdlib / base
import Control.Exception (SomeException, try)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text qualified as T

-- Grupo 2 — terceros
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON (..), decode, encode, object, withObject, (.:), (.:?), (.=))
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
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)

-- Grupo 3 — proyecto
import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.LevelDefinition (
  LevelDefinition,
  ResolvedBehaviour (..),
  parseBehaviourArchetype,
 )
import Domain.ValueObjects.Amplifier (identityAmplifier, mkAmplifier, unAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Multiplier (identityMultiplier, mkMultiplier, unMultiplier)
import UseCases.Ports.BehaviourResolverPort (
  BehaviourResolverPort (..),
  runNoResolver,
 )
import UseCases.ResolveBehaviours (resolveLevelBehaviours)

data ResolverEnv = ResolverEnv
  { reApiKey :: Text
  , reModel :: Text
  , reManager :: Manager
  , reBaseUrl :: String
  , reDebug :: Bool
  }

newtype AnthropicResolver a = AnthropicResolver
  {runAnthropicResolver :: ReaderT ResolverEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ResolverEnv)

instance BehaviourResolverPort AnthropicResolver where
  resolveBehaviourHint kind hint = do
    env <- ask
    liftIO (resolveOne env kind hint)

defaultModel :: Text
defaultModel = "claude-haiku-4-5"

-- | Resuelve presets del nivel vía API, o degrada a 'runNoResolver' sin key.
resolveLevelIO :: LevelDefinition -> IO LevelDefinition
resolveLevelIO def = do
  -- Key vacía o solo espacios → mismo camino que ausente (evita 401 por hint).
  mKey <- nonEmptyApiKey <$> lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> do
      hPutStrLn
        stderr
        "[behaviour-resolver] ANTHROPIC_API_KEY ausente o vacía; uso arquetipos por defecto."
      pure (runNoResolver (resolveLevelBehaviours def))
    Just key -> do
      mModel <- lookupEnv "WONDERBOY_RESOLVER_MODEL"
      mDebug <- lookupEnv "WONDERBOY_RESOLVER_DEBUG"
      manager <- newTlsManager
      let env =
            ResolverEnv
              { reApiKey = key
              , reModel = maybe defaultModel T.pack mModel
              , reManager = manager
              , reBaseUrl = "https://api.anthropic.com/v1/messages"
              , reDebug = maybe False (not . null) mDebug
              }
      debugLog
        env
        ("activo; modelo=" <> T.unpack (reModel env) <> " endpoint=" <> reBaseUrl env)
      runReaderT (runAnthropicResolver (resolveLevelBehaviours def)) env

nonEmptyApiKey :: Maybe String -> Maybe Text
nonEmptyApiKey raw = do
  s <- raw
  let trimmed = T.strip (T.pack s)
  if T.null trimmed then Nothing else Just trimmed

-- | 10 s: la resolución corre sincrónicamente al cargar nivel y bloquearía Gloss.
resolverTimeoutMicros :: Int
resolverTimeoutMicros = 10 * 1000 * 1000

debugLog :: ResolverEnv -> String -> IO ()
debugLog env msg
  | reDebug env = hPutStrLn stderr ("[behaviour-resolver:debug] " <> msg)
  | otherwise = pure ()

-- | Primer objeto JSON en texto con prosa o cercas markdown.
extractJsonObject :: Text -> Maybe Text
extractJsonObject t =
  let afterOpen = T.dropWhile (/= '{') t
      (beforeClose, _) = T.breakOnEnd "}" afterOpen
   in if T.null afterOpen || T.null beforeClose
        then Nothing
        else Just beforeClose

-- | Una consulta a la API; cualquier falla → 'Nothing' + warning (nunca aborta).
resolveOne :: ResolverEnv -> EnemyKind -> Text -> IO (Maybe ResolvedBehaviour)
resolveOne env kind hint = do
  debugLog env ("consultando: kind=" <> show kind <> " hint=" <> show hint)
  debugLog env ("prompt: " <> T.unpack (promptText kind hint))
  result <- try @SomeException $ do
    baseReq <- parseRequest (reBaseUrl env)
    let req =
          baseReq
            { method = "POST"
            , requestHeaders =
                [ ("x-api-key", encodeUtf8 (reApiKey env))
                , ("anthropic-version", "2023-06-01")
                , ("content-type", "application/json")
                ]
            , requestBody = RequestBodyLBS (encode body)
            , responseTimeout = responseTimeoutMicro resolverTimeoutMicros
            , redactHeaders = Set.fromList ["x-api-key"]
            }
    httpLbs req (reManager env)
  case result of
    Left err -> warn ("falla de red: " <> show err)
    Right resp
      | inRange2xx (statusCode (responseStatus resp)) -> do
          debugLog
            env
            ( "status "
                <> show (statusCode (responseStatus resp))
                <> "; cuerpo crudo: "
                <> previewBody (responseBody resp)
            )
          interpretBody (responseBody resp)
      | otherwise ->
          warn ("status inesperado: " <> show (statusCode (responseStatus resp)))
 where
  body =
    object
      [ "model" .= reModel env
      , "max_tokens" .= (64 :: Int)
      , "temperature" .= (0.0 :: Double)
      , "messages"
          .= [ object
                [ "role" .= ("user" :: Text)
                , "content" .= promptText kind hint
                ]
             ]
      ]

  previewBody :: BL.ByteString -> String
  previewBody = T.unpack . T.take 600 . decodeUtf8Lenient . BL.toStrict

  warn :: String -> IO (Maybe ResolvedBehaviour)
  warn msg = do
    hPutStrLn stderr ("[behaviour-resolver] " <> msg <> "; uso arquetipo por defecto.")
    pure Nothing

  interpretBody bs =
    case parseModelReply bs of
      Left err -> warn err
      Right reply ->
        case resolvedFromReply reply of
          Nothing -> warn ("arquetipo no reconocido: " <> T.unpack (rrArchetype reply))
          Just rb -> do
            debugLog env ("resuelto: " <> show (rbArchetype rb) <> " tuning=" <> showTuning (rbTuning rb))
            pure (Just rb)

  showTuning :: BehaviourTuning -> String
  showTuning tuning =
    "speed="
      <> show (unMultiplier (tuningSpeed tuning))
      <> " reach="
      <> show (unAmplifier (tuningReach tuning))
      <> " toughness="
      <> show (unAmplifier (tuningToughness tuning))

promptText :: EnemyKind -> Text -> Text
promptText kind hint =
  "Sos un diseñador de niveles de un plataformero 2D. Para un enemigo ("
    <> T.pack (show kind)
    <> ") con esta descripción, devolvé SOLO un objeto JSON (sin texto extra) con:\n"
    <> "  \"archetype\": \"patrol\" | \"chase\" | \"guard\" (la forma de moverse),\n"
    <> "  \"speed\": número (1.0 normal, <1 más lento, >1 más rápido),\n"
    <> "  \"reach\": número >= 1.0 (1.0 = alcance base del arquetipo, >1 detecta y persigue más lejos),\n"
    <> "  \"toughness\": número >= 1.0 (1.0 = vida base, >1 más resistente),\n"
    <> "Descripción: "
    <> hint

replyText :: AnthropicResponse -> Maybe Text
replyText resp = do
  block <- find ((== "text") . acType) (arContent resp)
  acText block

parseModelReply :: BL.ByteString -> Either String ResolverReply
parseModelReply bs =
  case decode @AnthropicResponse bs of
    Nothing -> Left "JSON de respuesta inesperado"
    Just resp ->
      case replyText resp of
        Nothing -> Left "respuesta sin texto"
        Just t ->
          case extractJsonObject t of
            Nothing -> Left "respuesta sin objeto JSON"
            Just jsonText ->
              case decode @ResolverReply (BL.fromStrict (encodeUtf8 jsonText)) of
                Nothing -> Left ("JSON del modelo no parseable: " <> T.unpack jsonText)
                Just reply -> Right reply

data ResolverReply = ResolverReply
  { rrArchetype :: Text
  , rrSpeed :: Maybe Double
  , rrReach :: Maybe Double
  , rrToughness :: Maybe Double
  }
  deriving (Eq, Show)

instance FromJSON ResolverReply where
  parseJSON =
    withObject "ResolverReply" $ \o ->
      ResolverReply
        <$> o .: "archetype"
        <*> o .:? "speed"
        <*> o .:? "reach"
        <*> o .:? "toughness"

-- | Mapeo puro; exportado para tests sin llamar a la API.
resolvedFromReply :: ResolverReply -> Maybe ResolvedBehaviour
resolvedFromReply r =
  case parseBehaviourArchetype (T.toLower (T.strip (rrArchetype r))) of
    Left _ -> Nothing
    Right arch ->
      Just
        ( ResolvedBehaviour
            arch
            ( BehaviourTuning
                (maybe identityMultiplier (mkMultiplier . realToFrac) (rrSpeed r))
                (maybe identityAmplifier (mkAmplifier . realToFrac) (rrReach r))
                (maybe identityAmplifier (mkAmplifier . realToFrac) (rrToughness r))
            )
        )

newtype AnthropicResponse = AnthropicResponse {arContent :: [AnthropicContent]}

-- | @text@ opcional: bloques @thinking@/@tool_use@ no rompen el decode.
data AnthropicContent = AnthropicContent
  { acType :: Text
  , acText :: Maybe Text
  }

instance FromJSON AnthropicResponse where
  parseJSON =
    withObject "AnthropicResponse" $ \o ->
      AnthropicResponse <$> o .: "content"

instance FromJSON AnthropicContent where
  parseJSON =
    withObject "AnthropicContent" $ \o ->
      AnthropicContent <$> o .: "type" <*> o .:? "text"

inRange2xx :: Int -> Bool
inRange2xx code = code >= 200 && code < 300
