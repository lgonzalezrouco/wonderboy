{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Adaptador concreto del puerto 'BehaviourResolverPort': resuelve el
@behaviourHint@ (texto libre del autor del nivel) a un 'BehaviourArchetype'
consultando a la API de Anthropic (Claude). Acá vive TODO el 'IO' del feature:
lectura de variables de entorno, creación del 'Manager' TLS y la llamada HTTP.

__Por qué un newtype y no una instancia sobre 'IO':__ el puerto se define en
@UseCases/@ y la orquestación ('UseCases.ResolveBehaviours') es genérica sobre la
mónada @m@. Implementar @instance BehaviourResolverPort IO@ sería una instancia
/huérfana/ (ni el typeclass ni 'IO' viven en este módulo) y además acoplaría el
puerto a 'IO'. En su lugar definimos 'AnthropicResolver' — un @ReaderT@ sobre 'IO'
que transporta la configuración de runtime — y le damos la instancia acá, donde el
newtype sí está definido. @UseCases/@ nunca importa este módulo.

__Degradación con gracia (alineada con la semántica de fallback del puerto):__
ninguna falla acá tumba la carga del nivel. Sin API key, falla de red, status
fuera de 2xx, JSON inesperado o respuesta no reconocida → 'Nothing' (más un
warning a 'stderr'); el build puro cae al default del kind y el juego sigue
jugable. Esto también mantiene el CI verde sin acceso a la red.
-}
module Adapters.BehaviourResolver (resolveLevelIO)
where

-- Grupo 1 — stdlib / base
import Control.Exception (SomeException, try)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import Data.Text qualified as T

-- Grupo 2 — terceros
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON (..), decode, encode, object, withObject, (.:), (.=))
import Network.HTTP.Client (
  Manager,
  RequestBody (RequestBodyLBS),
  Response (responseBody, responseStatus),
  httpLbs,
  method,
  parseRequest,
  requestBody,
  requestHeaders,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)

-- Grupo 3 — proyecto
import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.LevelDefinition (
  BehaviourArchetype,
  LevelDefinition,
  parseBehaviourArchetype,
 )
import UseCases.Ports.BehaviourResolverPort (
  BehaviourResolverPort (..),
  runNoResolver,
 )
import UseCases.ResolveBehaviours (resolveLevelBehaviours)

-- ---------------------------------------------------------------------------
-- Configuración de runtime y mónada del adapter
-- ---------------------------------------------------------------------------

{- | Configuración de runtime del adapter, resuelta una sola vez por carga de
nivel y transportada por el 'ReaderT' a cada consulta individual.

__Por qué `data` y no `newtype`:__ tiene varios campos, así que `newtype` (que
exige exactamente uno) no aplica.
-}
data ResolverEnv = ResolverEnv
  { reApiKey :: Text
  -- ^ API key de Anthropic (de @ANTHROPIC_API_KEY@); va en el header @x-api-key@.
  , reModel :: Text
  -- ^ Modelo a usar; 'defaultModel' salvo override por @WONDERBOY_RESOLVER_MODEL@.
  , reManager :: Manager
  -- ^ 'Manager' TLS reutilizado entre consultas (pooling de conexiones).
  , reBaseUrl :: String
  -- ^ Endpoint de la Messages API (string porque 'parseRequest' lo espera así).
  }

{- | Mónada concreta del adapter: @ReaderT ResolverEnv IO@.

El newtype evita una instancia huérfana de 'BehaviourResolverPort' sobre 'IO'
(ver doc del módulo) y le da un nombre corto a la pila. La maquinaria monádica
('Functor'..'MonadReader') se deriva con @GeneralizedNewtypeDeriving@ desde el
'ReaderT' subyacente, de modo que no hay que reimplementarla a mano.
-}
newtype AnthropicResolver a = AnthropicResolver
  {runAnthropicResolver :: ReaderT ResolverEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ResolverEnv)

{- | Instancia del puerto: cada pista se resuelve consultando a la API.

Lee el entorno con 'ask' y delega en 'resolveOne' (que vive en 'IO') vía
'liftIO'. 'resolveLevelBehaviours' se encarga del dedup, así que esta acción se
invoca una vez por par @(kind, hint)@ distinto.
-}
instance BehaviourResolverPort AnthropicResolver where
  resolveBehaviourHint kind hint = do
    env <- ask
    liftIO (resolveOne env kind hint)

-- ---------------------------------------------------------------------------
-- Punto de entrada
-- ---------------------------------------------------------------------------

{- | Modelo por defecto: barato y rápido, en el rol de "SLM" (small language model)
clasificador. Se puede sobreescribir con la variable @WONDERBOY_RESOLVER_MODEL@.
-}
defaultModel :: Text
defaultModel = "claude-haiku-4-5"

{- | Punto de entrada del adapter: resuelve los presets de comportamiento de un
nivel, llamando a la API o degradando a un no-op según el entorno.

Si no hay @ANTHROPIC_API_KEY@, se loguea un aviso y se usa 'runNoResolver' (el
resolver nulo puro): la 'LevelDefinition' vuelve sin presets nuevos y el build
cae a los defaults del kind. Si hay key, se arma el 'ResolverEnv' (con el modelo
override opcional y un 'Manager' TLS) y se corre la orquestación en
'AnthropicResolver'.
-}
resolveLevelIO :: LevelDefinition -> IO LevelDefinition
resolveLevelIO def = do
  mKey <- lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> do
      hPutStrLn
        stderr
        "[behaviour-resolver] ANTHROPIC_API_KEY ausente; uso arquetipos por defecto."
      pure (runNoResolver (resolveLevelBehaviours def))
    Just key -> do
      mModel <- lookupEnv "WONDERBOY_RESOLVER_MODEL"
      manager <- newTlsManager
      let env =
            ResolverEnv
              { reApiKey = T.pack key
              , reModel = maybe defaultModel T.pack mModel
              , reManager = manager
              , reBaseUrl = "https://api.anthropic.com/v1/messages"
              }
      runReaderT (runAnthropicResolver (resolveLevelBehaviours def)) env

-- ---------------------------------------------------------------------------
-- Consulta individual a la API
-- ---------------------------------------------------------------------------

{- | Resuelve una sola pista a un arquetipo consultando la Messages API.

Construye el request POST, lo envía atrapando cualquier excepción y parsea la
respuesta. /Cualquier/ desvío (excepción de red, status no-2xx, JSON inesperado,
texto no reconocido) se convierte en 'Nothing' más un warning a 'stderr': nunca
propaga una excepción que pudiera abortar la carga del nivel.
-}
resolveOne :: ResolverEnv -> EnemyKind -> Text -> IO (Maybe BehaviourArchetype)
resolveOne env kind hint = do
  -- `parseRequest` falla en `IO` si la URL es inválida; al ser una constante del
  -- código no debería ocurrir, pero lo cubrimos con el mismo `try` que la llamada.
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
            }
    httpLbs req (reManager env)
  case result of
    Left err -> warn ("falla de red: " <> show err)
    Right resp
      -- Solo 2xx trae un cuerpo que valga la pena parsear.
      | inRange2xx (statusCode (responseStatus resp)) ->
          interpretBody (responseBody resp)
      | otherwise ->
          warn ("status inesperado: " <> show (statusCode (responseStatus resp)))
 where
  -- Cuerpo JSON del request: pedimos UNA palabra, sin pensar de más.
  -- `max_tokens` chico y `temperature` 0 acotan el costo y hacen la salida
  -- determinista (mismo hint → misma palabra).
  body =
    object
      [ "model" .= reModel env
      , "max_tokens" .= (16 :: Int)
      , "temperature" .= (0 :: Int)
      , "messages"
          .= [ object
                [ "role" .= ("user" :: Text)
                , "content" .= promptText kind hint
                ]
             ]
      ]

  -- `try @SomeException (httpLbs ...)` ya garantiza no propagar; si algo falla,
  -- devolvemos `Nothing` con un aviso a stderr para que el operador lo vea.
  warn :: String -> IO (Maybe BehaviourArchetype)
  warn msg = do
    hPutStrLn stderr ("[behaviour-resolver] " <> msg <> "; uso arquetipo por defecto.")
    pure Nothing

  -- Parsea el cuerpo, extrae el primer bloque de texto y lo normaliza.
  interpretBody bs =
    case decode @AnthropicResponse bs of
      Nothing -> warn "JSON de respuesta inesperado"
      Just resp ->
        case firstWord resp of
          Nothing -> warn "respuesta sin texto"
          Just w ->
            case parseBehaviourArchetype w of
              Left _ -> warn ("arquetipo no reconocido: " <> T.unpack w)
              Right arch -> pure (Just arch)

{- | Prompt para el clasificador: instruye al modelo a responder EXACTAMENTE una
palabra (@patrol@, @chase@ o @guard@), sin puntuación ni explicación, e incluye
el contexto (tipo de enemigo) para acotar el espacio de respuestas válidas.
-}
promptText :: EnemyKind -> Text -> Text
promptText kind hint =
  "Clasificá el comportamiento de un enemigo ("
    <> T.pack (show kind)
    <> ") de un plataformero 2D. Respondé SOLO una palabra: patrol, chase o guard. Pista: "
    <> hint

{- | Extrae la primera palabra del primer bloque de texto de la respuesta,
normalizada a minúsculas y sin espacios alrededor.

Trabaja en la mónada 'Maybe': 'listToMaybe' sobre @content@ corta si no hay
bloques de texto, y 'listToMaybe' sobre @T.words@ toma la primera palabra (o
'Nothing' si el texto quedó vacío), tolerando respuestas que agreguen texto de
más pese al prompt.
-}
firstWord :: AnthropicResponse -> Maybe Text
firstWord resp = do
  block <- listToMaybe (arContent resp)
  listToMaybe (T.words (T.toLower (T.strip (acText block))))

-- ---------------------------------------------------------------------------
-- Tipos de la respuesta de la API (FromJSON parcial, solo lo que usamos)
-- ---------------------------------------------------------------------------

{- | Vista mínima de la respuesta de la Messages API: solo el array @content@.

Modelamos únicamente los campos que consumimos; el resto del JSON se ignora.
-}
newtype AnthropicResponse = AnthropicResponse {arContent :: [AnthropicContent]}

-- | Un bloque de @content@; solo nos interesa su campo @text@.
newtype AnthropicContent = AnthropicContent {acText :: Text}

instance FromJSON AnthropicResponse where
  parseJSON =
    withObject "AnthropicResponse" $ \o ->
      AnthropicResponse <$> o .: "content"

instance FromJSON AnthropicContent where
  parseJSON =
    withObject "AnthropicContent" $ \o ->
      AnthropicContent <$> o .: "text"

-- | Predicado: ¿el código de status HTTP está en el rango de éxito 2xx?
inRange2xx :: Int -> Bool
inRange2xx code = code >= 200 && code < 300
