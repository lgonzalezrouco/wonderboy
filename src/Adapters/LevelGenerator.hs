{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Adaptador de 'LevelGeneratorPort': genera 'LevelDefinition' vía la API de
Anthropic. Todo el 'IO' del feature vive acá.

'AnthropicGenerator' (@ReaderT GeneratorEnv IO@) evita una instancia huérfana sobre
'IO' y mantiene @UseCases/@ libre de este módulo. Cualquier falla degrada a
'Nothing'; @Frameworks/@ hace fallback al @level{N}.json@ fijo.
-}
module Adapters.LevelGenerator (generateCatalogIO)
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
import Data.Aeson (FromJSON (..), Value, decode, encode, object, withObject, (.:), (.:?), (.=))
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
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)

-- Grupo 3 — proyecto
import Adapters.LevelFile (readLevelFile)
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Model.LevelDefinition (LevelDefinition)
import Paths_wonderboy_hs (getDataFileName)
import UseCases.GenerateLevels (defaultProfiles, generateCatalog)
import UseCases.LoadLevel (decodeLevelDefinition)
import UseCases.Ports.LevelGeneratorPort (
  LevelGeneratorPort (..),
  LevelProfile (..),
  LevelRole (..),
  runNoGenerator,
 )

data GeneratorEnv = GeneratorEnv
  { geApiKey :: Text
  , geModel :: Text
  , geManager :: Manager
  , geBaseUrl :: String
  , geDebug :: Bool
  }

newtype AnthropicGenerator a = AnthropicGenerator
  {runAnthropicGenerator :: ReaderT GeneratorEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GeneratorEnv)

instance LevelGeneratorPort AnthropicGenerator where
  generateLevel profile = do
    env <- ask
    liftIO (generateOne env profile)

defaultModel :: Text
defaultModel = "claude-haiku-4-5"

-- | Genera el catálogo vía API, o degrada a 'runNoGenerator' sin key.
generateCatalogIO :: Maybe Text -> IO [Maybe LevelDefinition]
generateCatalogIO theme = do
  mKey <- nonEmptyApiKey <$> lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> do
      hPutStrLn
        stderr
        "[level-generator] ANTHROPIC_API_KEY ausente o vacía; uso niveles fijos."
      pure (runNoGenerator (generateCatalog (defaultProfiles theme)))
    Just key -> do
      mModel <- lookupEnv "WONDERBOY_GENERATOR_MODEL"
      mDebug <- lookupEnv "WONDERBOY_GENERATOR_DEBUG"
      manager <- newTlsManager
      let env =
            GeneratorEnv
              { geApiKey = key
              , geModel = maybe defaultModel T.pack mModel
              , geManager = manager
              , geBaseUrl = "https://api.anthropic.com/v1/messages"
              , geDebug = maybe False (not . null) mDebug
              }
      debugLog
        env
        ("activo; modelo=" <> T.unpack (geModel env) <> " endpoint=" <> geBaseUrl env)
      runReaderT (runAnthropicGenerator (generateCatalog (defaultProfiles theme))) env

nonEmptyApiKey :: Maybe String -> Maybe Text
nonEmptyApiKey raw = do
  s <- raw
  let trimmed = T.strip (T.pack s)
  if T.null trimmed then Nothing else Just trimmed

debugLog :: GeneratorEnv -> String -> IO ()
debugLog env msg
  | geDebug env = hPutStrLn stderr ("[level-generator:debug] " <> msg)
  | otherwise = pure ()

-- | Una consulta a la API por perfil, con un reintento; cualquier falla → 'Nothing'.
generateOne :: GeneratorEnv -> LevelProfile -> IO (Maybe LevelDefinition)
generateOne env profile = do
  debugLog
    env
    ( "generando: índice="
        <> show (profileIndex profile)
        <> " rol="
        <> show (profileRole profile)
    )
  exampleText <- loadExample env profile
  let prompt = promptText profile exampleText
  debugLog env ("prompt: " <> T.unpack prompt)
  first <- attempt env prompt
  case first of
    Just{} -> pure first
    Nothing -> do
      debugLog env "primer intento fallido; reintentando una vez"
      second <- attempt env prompt
      case second of
        Just{} -> pure second
        Nothing -> do
          warn
            ( "no se pudo generar el nivel "
                <> show (profileIndex profile)
                <> " tras reintentar"
            )
          pure Nothing

-- | Few-shot desde @levels/level{index+1}.json@; 'Nothing' si no se puede leer.
loadExample :: GeneratorEnv -> LevelProfile -> IO (Maybe Text)
loadExample env profile = do
  let relPath = "levels/level" <> show (profileIndex profile + 1) <> ".json"
  path <- getDataFileName relPath
  readResult <- readLevelFile path
  case readResult of
    Left err -> do
      debugLog env ("few-shot no disponible (" <> err <> "); sigo sin ejemplo")
      pure Nothing
    Right txt -> pure (Just txt)

attempt :: GeneratorEnv -> Text -> IO (Maybe LevelDefinition)
attempt env prompt = do
  result <- try @SomeException $ do
    baseReq <- parseRequest (geBaseUrl env)
    let req =
          baseReq
            { method = "POST"
            , requestHeaders =
                [ ("x-api-key", encodeUtf8 (geApiKey env))
                , ("anthropic-version", "2023-06-01")
                , ("content-type", "application/json")
                ]
            , requestBody = RequestBodyLBS (encode body)
            , -- Evita filtrar la key si `show` del Request aparece en stderr.
              redactHeaders = Set.fromList ["x-api-key"]
            }
    httpLbs req (geManager env)
  case result of
    Left err -> do
      warn ("falla de red: " <> show err)
      pure Nothing
    Right resp
      | inRange2xx (statusCode (responseStatus resp)) -> do
          debugLog
            env
            ( "status "
                <> show (statusCode (responseStatus resp))
                <> "; cuerpo crudo: "
                <> previewBody (responseBody resp)
            )
          interpretBody env (responseBody resp)
      | otherwise -> do
          warn ("status inesperado: " <> show (statusCode (responseStatus resp)))
          pure Nothing
 where
  body :: Value
  body =
    object
      [ "model" .= geModel env
      , "max_tokens" .= (4096 :: Int)
      , "temperature" .= (0.9 :: Double)
      , "messages"
          .= [ object
                [ "role" .= ("user" :: Text)
                , "content" .= prompt
                ]
             ]
      ]

interpretBody :: GeneratorEnv -> BL.ByteString -> IO (Maybe LevelDefinition)
interpretBody env bs =
  case decode @AnthropicResponse bs of
    Nothing -> do
      debugLog env "JSON de respuesta inesperado"
      pure Nothing
    Just resp ->
      case firstText resp of
        Nothing -> do
          debugLog env "respuesta sin texto"
          pure Nothing
        Just raw -> do
          let levelJson = stripCodeFences raw
          debugLog env ("JSON del nivel extraído: " <> preview levelJson)
          case decodeLevelDefinition levelJson of
            Left err -> do
              debugLog env ("decode falló: " <> show err)
              pure Nothing
            Right def ->
              case buildWorld def of
                Left err -> do
                  debugLog env ("build falló: " <> show err)
                  pure Nothing
                Right _ -> do
                  debugLog env "nivel válido (decode + build OK)"
                  pure (Just def)

warn :: String -> IO ()
warn msg =
  hPutStrLn stderr ("[level-generator] " <> msg <> "; uso nivel fijo.")

preview :: Text -> String
preview = T.unpack . T.take 600

previewBody :: BL.ByteString -> String
previewBody = preview . decodeUtf8Lenient . BL.toStrict

-- | Quita cercas markdown si el modelo las agregó pese al prompt.
stripCodeFences :: Text -> Text
stripCodeFences raw =
  let trimmed = T.strip raw
   in if "```" `T.isPrefixOf` trimmed
        then
          let afterOpen = T.drop 1 (T.dropWhile (/= '\n') trimmed)
              withoutClose = fst (T.breakOn "```" afterOpen)
           in T.strip withoutClose
        else trimmed

firstText :: AnthropicResponse -> Maybe Text
firstText resp = do
  block <- find ((== "text") . acType) (arContent resp)
  acText block

promptText :: LevelProfile -> Maybe Text -> Text
promptText profile mExample =
  T.intercalate
    "\n\n"
    ( [ intro
      , schemaSection
      , roleSection (profileRole profile)
      , playabilitySection
      ]
        <> themeSection (profileTheme profile)
        <> exampleSection mExample
        <> [closing]
    )
 where
  intro =
    "Sos un diseñador de niveles para un plataformero 2D estilo Wonder Boy. "
      <> "Generá UN nivel jugable y devolvé EXCLUSIVAMENTE un objeto JSON válido "
      <> "que cumpla el schema de abajo. NO incluyas markdown, NO uses bloques de "
      <> "código, NO agregues explicaciones ni texto fuera del JSON: tu respuesta "
      <> "completa debe ser el objeto JSON y nada más."

  schemaSection =
    T.intercalate
      "\n"
      [ "Schema del nivel (todas las posiciones son objetos {\"x\": Float, \"y\": Float} con ancla bottom-left):"
      , "- minScore (Int >= 0): puntaje mínimo para completar el nivel."
      , "- spawn ({x,y}): posición inicial del jugador; debe quedar sobre una plataforma."
      , "- platforms (array de {pos, width, height}): plataformas fijas. Incluí un piso continuo y dos paredes de límite (height alto) SOLO en los extremos del nivel: una a la izquierda del spawn (en la x mínima) y otra a la derecha del exit (en la x máxima). NO agregues paredes en el interior del nivel."
      , "- movingPlatforms (array de {id, pos, width, height, endA, endB, speed, startTowardB}): plataformas que oscilan entre endA y endB."
      , "- enemies (array de {id, kind, pos}): enemigos. kind es uno de: snail, bat, golem, archer, bossGolem, bossBat. NO incluyas NUNCA los campos behaviourPreset ni behaviourHint en los enemigos, AUNQUE el ejemplo de abajo los traiga: ignoralos por completo (cada enemigo usa su comportamiento por defecto)."
      , "- pickups (array de {id, pos, value}): gemas; value es un Int de puntaje."
      , "- fallingHazards (array de {id, pos, width, height, fallSpeed, loopDelay opcional}): peligros que caen; fallSpeed > 0."
      , "- crumblingPlatforms (array de {id, pos, width, height}): plataformas que se desmoronan al pisarlas."
      , "- bossArena (opcional, {left, right}): límites en X de la arena del jefe; left < right."
      , "- exit ({pos, width, height}): zona de salida; debe quedar APOYADA sobre una plataforma fija (con piso justo debajo), nunca flotando en el aire."
      , "Tipos numéricos: minScore, todos los id, value y loopDelay son ENTEROS (sin decimales). Los demás números (x, y, width, height, speed, fallSpeed) pueden ser decimales."
      ]

  -- Columnas altas y angostas en el interior se renderizan invisibles y encierran al jugador.
  playabilitySection =
    T.intercalate
      "\n"
      [ "Reglas de jugabilidad (obligatorias):"
      , "- El spawn del jugador debe quedar parado sobre una plataforma fija (debe haber piso justo debajo del spawn)."
      , "- El piso debe ser CONTINUO desde el spawn hasta el exit: nada de huecos por los que el jugador caiga al vacío en el recorrido principal."
      , "- El exit debe estar APOYADO sobre una plataforma fija: tiene que existir una plataforma cuya cara superior quede a la altura de la base del exit (exit.pos.y igual a platform.pos.y + platform.height) y que cubra el rango horizontal del exit. NUNCA dejes el exit flotando."
      , "- La salida (exit) debe ser alcanzable caminando y saltando entre plataformas desde el spawn."
      , "- Las ÚNICAS plataformas verticales (más altas que anchas) deben ser las dos paredes de límite en los extremos del nivel: la izquierda antes del spawn y la derecha después del exit. NO pongas columnas altas y angostas en el interior ni cerca del spawn: el juego las convierte en barreras invisibles que encierran al jugador."
      , "- Las plataformas del interior (pisos y repisas) deben ser anchas y bajas: su width siempre claramente mayor que su height, nunca columnas."
      , "- Los ids deben ser únicos DENTRO de cada tipo (enemies, pickups, movingPlatforms, etc.), empezando en 1."
      , "- Usá coordenadas razonables, en los mismos rangos que el ejemplo (x de unos -280 a algunos miles, y de 0 a ~200 para el contenido jugable)."
      , "- minScore no debe superar la suma de los value de todos los pickups."
      ]

  closing =
    "Generá AHORA el objeto JSON del nivel, distinto del ejemplo, respetando "
      <> "todas las reglas. Recordá: la respuesta debe ser SOLO el JSON."

roleSection :: LevelRole -> Text
roleSection role = case role of
  IntroRole ->
    T.intercalate
      "\n"
      [ "Rol del nivel: INTRODUCTORIO (el primero de la partida)."
      , "- Usá solo plataformas fijas, enemigos básicos (snail, bat, golem, archer), pickups y exit."
      , "- NO incluyas movingPlatforms, fallingHazards, crumblingPlatforms ni bossArena."
      , "- Mantené el layout simple y la dificultad baja: pocos enemigos, saltos cómodos."
      ]
  ChallengeRole ->
    T.intercalate
      "\n"
      [ "Rol del nivel: DESAFÍO (segundo nivel, dificultad media)."
      , "- Incluí plataformas fijas, enemigos básicos (snail, bat, golem, archer), pickups y exit."
      , "- Agregá algunas movingPlatforms y/o fallingHazards y/o crumblingPlatforms para subir la dificultad."
      , "- NO incluyas bossArena ni enemigos boss."
      ]
  BossRole ->
    T.intercalate
      "\n"
      [ "Rol del nivel: JEFE (último nivel)."
      , "- Incluí una bossArena ({left, right}) y EXACTAMENTE UN enemigo boss (kind bossGolem o bossBat)."
      , "- Podés sumar plataformas fijas, móviles, hazards y pickups para llegar a la arena."
      , "- El enemigo boss debe quedar dentro del rango [left, right] de la bossArena."
      , "- No incluyas más de un enemigo boss."
      ]

themeSection :: Maybe Text -> [Text]
themeSection Nothing = []
themeSection (Just theme) =
  [ "Tema solicitado por el usuario: aplicá esta directiva temática al diseño del "
      <> "nivel (nombres, disposición y atmósfera implícita): "
      <> theme
  ]

exampleSection :: Maybe Text -> [Text]
exampleSection Nothing = []
exampleSection (Just example) =
  [ "Este es un ejemplo de formato y de un nivel jugable. Es SOLO una referencia "
      <> "de estructura y rangos de coordenadas: NO lo copies, generá un layout "
      <> "DISTINTO (otras plataformas, otros enemigos, otras posiciones).\n\n"
      <> example
  ]

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
