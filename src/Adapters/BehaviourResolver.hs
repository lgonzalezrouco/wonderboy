{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Adaptador concreto del puerto 'BehaviourResolverPort': resuelve el
@behaviourHint@ (texto libre del autor del nivel) a un 'ResolvedBehaviour'
(arquetipo + multiplicadores de gameplay) consultando a la API de Anthropic
(Claude). Acá vive TODO el 'IO' del feature: lectura de variables de entorno,
creación del 'Manager' TLS y la llamada HTTP.

__Por qué un newtype y no una instancia sobre 'IO':__ el puerto se define en
@UseCases/@ y la orquestación ('UseCases.ResolveBehaviours') es genérica sobre la
mónada @m@. Implementar @instance BehaviourResolverPort IO@ sería una instancia
/huérfana/ (ni el typeclass ni 'IO' viven en este módulo) y además acoplaría el
puerto a 'IO'. En su lugar definimos 'AnthropicResolver' — un @ReaderT@ sobre 'IO'
que transporta la configuración de runtime — y le damos la instancia acá, donde el
newtype sí está definido. @UseCases/@ nunca importa este módulo.

__Degradación con gracia (alineada con la semántica de fallback del puerto):__
ninguna falla acá tumba la carga del nivel. Sin API key, falla de red, status
fuera de 2xx, JSON inesperado, arquetipo no reconocido o número inválido →
'Nothing' (más un warning a 'stderr'); cada multiplicador ausente o fuera de rango
cae a 1.0 vía 'mkMultiplier'; el build puro cae al default del kind y el juego
sigue jugable. Esto también mantiene el CI verde sin acceso a la red.
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
import Domain.ValueObjects.Amplifier (Amplifier, identityAmplifier, mkAmplifier, unAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Multiplier (Multiplier, identityMultiplier, mkMultiplier, unMultiplier)
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
  , reDebug :: Bool
  -- ^ Modo debug: si está activo (@WONDERBOY_RESOLVER_DEBUG@ no vacío) cada
  -- consulta vuelca trazas detalladas a 'stderr' (par resuelto, prompt, cuerpo
  -- crudo y arquetipo). Apagado por defecto.
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
  -- `lookupEnv` devuelve `Just ""` si la variable está definida pero vacía (p. ej.
  -- `ANTHROPIC_API_KEY=` en el shell o un `.env` sin valor). 'nonEmptyApiKey' trata
  -- vacío —o solo espacios— igual que ausente: con una key vacía cada pista
  -- dispararía un request que la API rechaza (401), uno por hint. Degradamos una
  -- sola vez a 'runNoResolver' (el mismo camino que cuando la variable falta).
  mKey <- nonEmptyApiKey <$> lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> do
      hPutStrLn
        stderr
        "[behaviour-resolver] ANTHROPIC_API_KEY ausente o vacía; uso arquetipos por defecto."
      pure (runNoResolver (resolveLevelBehaviours def))
    Just key -> do
      mModel <- lookupEnv "WONDERBOY_RESOLVER_MODEL"
      -- Modo debug opcional: con `WONDERBOY_RESOLVER_DEBUG` seteada a cualquier
      -- valor no vacío se vuelcan trazas detalladas a stderr. Apagado por defecto,
      -- así la salida normal (y el CI) quedan limpios.
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
      -- Primera traza: confirma que el resolver está activo y con qué modelo apunta.
      debugLog
        env
        ("activo; modelo=" <> T.unpack (reModel env) <> " endpoint=" <> reBaseUrl env)
      runReaderT (runAnthropicResolver (resolveLevelBehaviours def)) env

{- | Normaliza la API key leída del entorno: 'Nothing' si está ausente, vacía o
compuesta solo por espacios; 'Just' con la key recortada en otro caso.

Distingue "definida con valor real" de "definida pero vacía" ('Just ""', que
'lookupEnv' devuelve para @ANTHROPIC_API_KEY=@). Recortar también evita que
espacios accidentales rompan el header @x-api-key@.
-}
nonEmptyApiKey :: Maybe String -> Maybe Text
nonEmptyApiKey raw = do
  s <- raw
  let trimmed = T.strip (T.pack s)
  if T.null trimmed then Nothing else Just trimmed

{- | Timeout de respuesta por consulta a la API, en microsegundos (10 s).

La resolución corre __sincrónicamente__ al cargar un nivel, incluido el cambio o
reinicio de nivel dentro del event handler de Gloss. Sin un límite explícito, una
API lenta o inalcanzable bloquearía la ventana hasta el default del manager (30 s)
__por cada pista distinta__. Al vencer, 'httpLbs' lanza una 'HttpException' que el
'try' de 'resolveOne' captura y degrada a 'Nothing' (arquetipo por defecto).
-}
resolverTimeoutMicros :: Int
resolverTimeoutMicros = 10 * 1000 * 1000

{- | Traza de depuración a 'stderr', condicionada al flag 'reDebug'.

Con @WONDERBOY_RESOLVER_DEBUG@ activa escribe una línea con prefijo
@[behaviour-resolver:debug]@; en operación normal es un no-op silencioso. __Nunca__
incluye 'reApiKey', así que las trazas son seguras de pegar en un reporte. El
prefijo las distingue de los warnings de fallback (@[behaviour-resolver]@).
-}
debugLog :: ResolverEnv -> String -> IO ()
debugLog env msg
  | reDebug env = hPutStrLn stderr ("[behaviour-resolver:debug] " <> msg)
  | otherwise = pure ()

-- ---------------------------------------------------------------------------
-- Extracción de JSON del texto del modelo
-- ---------------------------------------------------------------------------

{- | Extrae el primer objeto JSON de un texto que puede venir envuelto en cercas
markdown (@```json ... ```@) o con prosa alrededor: toma el substring desde el
primer @{@ hasta el último @}@. Devuelve 'Nothing' si no hay un par de llaves.
Endurece el happy path ante modelos que no respetan "respondé SOLO JSON".
-}
extractJsonObject :: Text -> Maybe Text
extractJsonObject t =
  let afterOpen = T.dropWhile (/= '{') t
      (beforeClose, _) = T.breakOnEnd "}" afterOpen
   in if T.null afterOpen || T.null beforeClose
        then Nothing
        else Just beforeClose

-- ---------------------------------------------------------------------------
-- Consulta individual a la API
-- ---------------------------------------------------------------------------

{- | Resuelve una sola pista a un arquetipo consultando la Messages API.

Construye el request POST, lo envía atrapando cualquier excepción y parsea la
respuesta. /Cualquier/ desvío (excepción de red, status no-2xx, JSON inesperado,
texto no reconocido) se convierte en 'Nothing' más un warning a 'stderr': nunca
propaga una excepción que pudiera abortar la carga del nivel.
-}
resolveOne :: ResolverEnv -> EnemyKind -> Text -> IO (Maybe ResolvedBehaviour)
resolveOne env kind hint = do
  -- Trazas de entrada (solo con debug on): qué par se resuelve y con qué prompt.
  -- Ver el prompt exacto ayuda a entender por qué el modelo respondió lo que respondió.
  debugLog env ("consultando: kind=" <> show kind <> " hint=" <> show hint)
  debugLog env ("prompt: " <> T.unpack (promptText kind hint))
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
            , -- Timeout de respuesta explícito (ver 'resolverTimeoutMicros'): acota
              -- cuánto puede bloquear la ventana de Gloss una API lenta o caída, ya
              -- que la resolución corre sincrónicamente al cargar/cambiar de nivel.
              responseTimeout = responseTimeoutMicro resolverTimeoutMicros
            , -- La API key viaja en el header `x-api-key`. `redactHeaders` hace que
              -- el `Show` del `Request` (que puede aparecer dentro de una
              -- `HttpException` y terminar en stderr vía `show err`) lo enmascare,
              -- evitando filtrar el secreto en los logs de error.
              redactHeaders = Set.fromList ["x-api-key"]
            }
    httpLbs req (reManager env)
  case result of
    Left err -> warn ("falla de red: " <> show err)
    Right resp
      -- Solo 2xx trae un cuerpo que valga la pena parsear.
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
  -- Cuerpo JSON del request: pedimos un objeto JSON con arquetipo y tres números.
  -- `max_tokens` en 64 es suficiente para el objeto y `temperature` 0 hace la
  -- salida determinista (mismo hint → mismo JSON).
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

  -- Preview legible (UTF-8 tolerante, truncada) del cuerpo crudo de la respuesta,
  -- para las trazas de debug. Truncar evita volcar respuestas largas a la consola.
  previewBody :: BL.ByteString -> String
  previewBody = T.unpack . T.take 600 . decodeUtf8Lenient . BL.toStrict

  -- `try @SomeException (httpLbs ...)` ya garantiza no propagar; si algo falla,
  -- devolvemos `Nothing` con un aviso a stderr para que el operador lo vea.
  warn :: String -> IO (Maybe ResolvedBehaviour)
  warn msg = do
    hPutStrLn stderr ("[behaviour-resolver] " <> msg <> "; uso arquetipo por defecto.")
    pure Nothing

  -- Parsea el cuerpo, extrae el texto del modelo y lo decodifica como
  -- 'ResolverReply'. Primero extrae el objeto JSON del texto (puede venir
  -- envuelto en cercas markdown o con prosa), luego decodifica. Cada paso
  -- deja una traza de debug para seguir en vivo la decisión del clasificador.
  interpretBody bs =
    case decode @AnthropicResponse bs of
      Nothing -> warn "JSON de respuesta inesperado"
      Just resp ->
        case replyText resp of
          Nothing -> warn "respuesta sin texto"
          Just t ->
            case extractJsonObject t of
              Nothing -> warn "respuesta sin objeto JSON"
              Just jsonText ->
                case decode @ResolverReply (BL.fromStrict (encodeUtf8 jsonText)) of
                  Nothing -> warn ("JSON del modelo no parseable: " <> T.unpack jsonText)
                  Just reply ->
                    case resolvedFromReply reply of
                      Nothing -> warn ("arquetipo no reconocido: " <> T.unpack (rrArchetype reply))
                      Just rb -> do
                        debugLog env ("resuelto: " <> show (rbArchetype rb) <> " tuning=" <> showTuning (rbTuning rb))
                        pure (Just rb)

  -- Helper de debug: muestra los tres multiplicadores sin exponer la API key.
  showTuning :: BehaviourTuning -> String
  showTuning tuning =
    "speed="
      <> show (unMultiplier (tuningSpeed tuning))
      <> " reach="
      <> show (unAmplifier (tuningReach tuning))
      <> " toughness="
      <> show (unAmplifier (tuningToughness tuning))

{- | Prompt para el clasificador: pide UN objeto JSON con el arquetipo y los tres
multiplicadores de gameplay. @speed@ admite @<1@ (más lento) y @>1@ (más rápido); @reach@ y
@toughness@ son @>= 1.0@ (1.0 = base del arquetipo; solo suben). Incluye el tipo de enemigo
como contexto para que el modelo ajuste sus sugerencias.
-}
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

{- | Texto del primer bloque de tipo @"text"@ de la respuesta del modelo.

Trabaja en la mónada 'Maybe': 'find' toma el primer bloque con @type == "text"@
(ignorando @thinking@, @tool_use@ u otros que no traen texto) y desempaqueta su
@text@ opcional. El texto completo se pasa luego a 'decode' para parsear el JSON
que devolvió el modelo.
-}
replyText :: AnthropicResponse -> Maybe Text
replyText resp = do
  block <- find ((== "text") . acType) (arContent resp)
  acText block

-- ---------------------------------------------------------------------------
-- DTO de la respuesta del modelo y mapeo puro a ResolvedBehaviour
-- ---------------------------------------------------------------------------

-- | Vista de la respuesta del modelo: arquetipo (texto) + 3 factores opcionales.
data ResolverReply = ResolverReply
  { rrArchetype :: Text
  -- ^ Nombre del arquetipo tal como lo devolvió el modelo (@"patrol"@, @"chase"@, @"guard"@).
  , rrSpeed :: Maybe Double
  -- ^ Multiplicador de velocidad sugerido por el modelo; 'Nothing' si ausente en el JSON.
  , rrReach :: Maybe Double
  -- ^ Multiplicador de alcance sugerido por el modelo; 'Nothing' si ausente en el JSON.
  , rrToughness :: Maybe Double
  -- ^ Multiplicador de resistencia sugerido por el modelo; 'Nothing' si ausente en el JSON.
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

{- | Mapeo puro respuesta → 'ResolvedBehaviour'. El arquetipo debe ser reconocible (si no,
'Nothing' y el build cae al default del kind); @speed@ ausente/raro cae a 1.0 vía
'mkMultiplier', y @reach@/@toughness@ ausentes, raros o por debajo de 1.0 caen a 1.0 vía
'mkAmplifier' (solo amplifican).

El módulo exporta esta función para que el test-suite la verifique directamente, sin
necesidad de invocar la API.
-}
resolvedFromReply :: ResolverReply -> Maybe ResolvedBehaviour
resolvedFromReply r =
  case parseBehaviourArchetype (T.toLower (T.strip (rrArchetype r))) of
    Left _ -> Nothing
    Right arch -> Just (ResolvedBehaviour arch tuning)
 where
  tuning =
    BehaviourTuning (mulSpeed (rrSpeed r)) (amp (rrReach r)) (amp (rrToughness r))
  mulSpeed :: Maybe Double -> Multiplier
  mulSpeed = maybe identityMultiplier (mkMultiplier . realToFrac)
  amp :: Maybe Double -> Amplifier
  amp = maybe identityAmplifier (mkAmplifier . realToFrac)

-- ---------------------------------------------------------------------------
-- Tipos de la respuesta de la API (FromJSON parcial, solo lo que usamos)
-- ---------------------------------------------------------------------------

{- | Vista mínima de la respuesta de la Messages API: solo el array @content@.

Modelamos únicamente los campos que consumimos; el resto del JSON se ignora.
-}
newtype AnthropicResponse = AnthropicResponse {arContent :: [AnthropicContent]}

{- | Un bloque del array @content@. Modelamos el discriminador @type@ y un @text@
/opcional/: la Messages API puede devolver bloques sin texto (p. ej. @thinking@
o @tool_use@, según el modelo configurado), y un 'FromJSON' que exigiera @text@
haría fallar el decode de __toda__ la respuesta. Con @text@ opcional esos bloques
se parsean sin romper y luego se descartan filtrando por @type == "text"@.
-}
data AnthropicContent = AnthropicContent
  { acType :: Text
  -- ^ Discriminador del bloque (@"text"@, @"thinking"@, @"tool_use"@, …).
  , acText :: Maybe Text
  -- ^ Texto del bloque cuando @type == "text"@; 'Nothing' en los demás.
  }

instance FromJSON AnthropicResponse where
  parseJSON =
    withObject "AnthropicResponse" $ \o ->
      AnthropicResponse <$> o .: "content"

instance FromJSON AnthropicContent where
  parseJSON =
    withObject "AnthropicContent" $ \o ->
      AnthropicContent <$> o .: "type" <*> o .:? "text"

-- | Predicado: ¿el código de status HTTP está en el rango de éxito 2xx?
inRange2xx :: Int -> Bool
inRange2xx code = code >= 200 && code < 300
