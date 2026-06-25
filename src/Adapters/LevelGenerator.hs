{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Adaptador concreto del puerto 'LevelGeneratorPort': genera la
'LevelDefinition' de cada nivel de la partida pidiéndole a la API de Anthropic
(Claude) que devuelva el JSON del nivel. Acá vive TODO el 'IO' del feature de
generación: lectura de variables de entorno, creación del 'Manager' TLS, lectura
del few-shot de disco y la llamada HTTP.

__Por qué un newtype y no una instancia sobre 'IO':__ es el mismo razonamiento
que en 'Adapters.BehaviourResolver'. El puerto se define en @UseCases/@ y la
orquestación ('UseCases.GenerateLevels') es genérica sobre la mónada @m@.
Implementar @instance LevelGeneratorPort IO@ sería una instancia /huérfana/ (ni
el typeclass ni 'IO' viven acá) y además acoplaría el puerto a 'IO'. En su lugar
definimos 'AnthropicGenerator' — un @ReaderT@ sobre 'IO' que transporta la
configuración de runtime — y le damos la instancia acá, donde el newtype sí está
definido. @UseCases/@ nunca importa este módulo.

__Degradación con gracia (alineada con la semántica de fallback del puerto):__
ninguna falla acá tumba la carga del catálogo. Sin API key, falla de red, status
fuera de 2xx, JSON inesperado, decode o build fallido (tras /un/ reintento) → el
nivel queda en 'Nothing' (más un warning a 'stderr'); el llamador en
@Frameworks/@ hace el fallback granular al @level{N}.json@ fijo y el juego sigue
jugable. Esto también mantiene el CI verde sin acceso a la red.
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

-- ---------------------------------------------------------------------------
-- Configuración de runtime y mónada del adapter
-- ---------------------------------------------------------------------------

{- | Configuración de runtime del adapter, resuelta una sola vez por catálogo y
transportada por el 'ReaderT' a cada generación individual.

__Por qué `data` y no `newtype`:__ tiene varios campos, así que `newtype` (que
exige exactamente uno) no aplica.
-}
data GeneratorEnv = GeneratorEnv
  { geApiKey :: Text
  -- ^ API key de Anthropic (de @ANTHROPIC_API_KEY@); va en el header @x-api-key@.
  , geModel :: Text
  -- ^ Modelo a usar; 'defaultModel' salvo override por @WONDERBOY_GENERATOR_MODEL@.
  , geManager :: Manager
  -- ^ 'Manager' TLS reutilizado entre niveles (pooling de conexiones).
  , geBaseUrl :: String
  -- ^ Endpoint de la Messages API (string porque 'parseRequest' lo espera así).
  , geDebug :: Bool
  -- ^ Modo debug: si está activo (@WONDERBOY_GENERATOR_DEBUG@ no vacío) cada
  -- generación vuelca trazas detalladas a 'stderr' (perfil, prompt, cuerpo
  -- crudo y resultado de validación). Apagado por defecto.
  }

{- | Mónada concreta del adapter: @ReaderT GeneratorEnv IO@.

El newtype evita una instancia huérfana de 'LevelGeneratorPort' sobre 'IO' (ver
doc del módulo) y le da un nombre corto a la pila. La maquinaria monádica
('Functor'..'MonadReader') se deriva con @GeneralizedNewtypeDeriving@ desde el
'ReaderT' subyacente, de modo que no hay que reimplementarla a mano.
-}
newtype AnthropicGenerator a = AnthropicGenerator
  {runAnthropicGenerator :: ReaderT GeneratorEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GeneratorEnv)

{- | Instancia del puerto: cada perfil se genera consultando a la API.

Lee el entorno con 'ask' y delega en 'generateOne' (que vive en 'IO') vía
'liftIO'. 'generateCatalog' itera sobre los perfiles, así que esta acción se
invoca una vez por nivel del catálogo.
-}
instance LevelGeneratorPort AnthropicGenerator where
  generateLevel profile = do
    env <- ask
    liftIO (generateOne env profile)

-- ---------------------------------------------------------------------------
-- Punto de entrada
-- ---------------------------------------------------------------------------

{- | Modelo por defecto: barato y rápido, suficiente para emitir el JSON de un
nivel a partir del schema y el few-shot. Se puede sobreescribir con la variable
@WONDERBOY_GENERATOR_MODEL@.
-}
defaultModel :: Text
defaultModel = "claude-haiku-4-5"

{- | Punto de entrada del adapter: genera el catálogo de niveles de una partida,
llamando a la API o degradando a un no-op según el entorno.

Si no hay @ANTHROPIC_API_KEY@, se loguea un aviso y se usa 'runNoGenerator' (el
generador nulo puro): el catálogo vuelve como una lista de puros 'Nothing' y
@Frameworks/@ cae al catálogo de archivos fijos. Si hay key, se arma el
'GeneratorEnv' (con el modelo override opcional, el flag de debug y un 'Manager'
TLS) y se corre la orquestación en 'AnthropicGenerator'.

El argumento es el tema opcional del usuario (@WONDERBOY_WORLD_PROMPT@), que
'defaultProfiles' propaga a los tres perfiles.
-}
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
      -- Primera traza: confirma que el generador está activo y con qué modelo apunta.
      debugLog
        env
        ("activo; modelo=" <> T.unpack (geModel env) <> " endpoint=" <> geBaseUrl env)
      runReaderT (runAnthropicGenerator (generateCatalog (defaultProfiles theme))) env

nonEmptyApiKey :: Maybe String -> Maybe Text
nonEmptyApiKey raw = do
  s <- raw
  let trimmed = T.strip (T.pack s)
  if T.null trimmed then Nothing else Just trimmed

{- | Traza de depuración a 'stderr', condicionada al flag 'geDebug'.

Con @WONDERBOY_GENERATOR_DEBUG@ activa escribe una línea con prefijo
@[level-generator:debug]@; en operación normal es un no-op silencioso. __Nunca__
incluye 'geApiKey', así que las trazas son seguras de pegar en un reporte. El
prefijo las distingue de los warnings de fallback (@[level-generator]@).
-}
debugLog :: GeneratorEnv -> String -> IO ()
debugLog env msg
  | geDebug env = hPutStrLn stderr ("[level-generator:debug] " <> msg)
  | otherwise = pure ()

-- ---------------------------------------------------------------------------
-- Generación individual de un nivel
-- ---------------------------------------------------------------------------

{- | Genera un único nivel a partir de su perfil, con un reintento.

Arma el prompt (schema + few-shot + reglas del rol) y pide el JSON a la API. La
respuesta se valida con el mismo pipeline puro que la carga de niveles fijos
('decodeLevelDefinition' seguido de 'buildWorld'), así un nivel generado que pasa
es indistinguible de uno autoral. /Cualquier/ desvío (excepción de red, status
no-2xx, JSON inesperado, decode o build fallido) dispara un único reintento; si
el reintento también falla, devuelve 'Nothing' más un warning a 'stderr'. Nunca
propaga una excepción que pudiera abortar la carga del catálogo.
-}
generateOne :: GeneratorEnv -> LevelProfile -> IO (Maybe LevelDefinition)
generateOne env profile = do
  -- Traza de entrada (solo con debug on): qué perfil se está generando.
  debugLog
    env
    ( "generando: índice="
        <> show (profileIndex profile)
        <> " rol="
        <> show (profileRole profile)
    )
  -- Few-shot: el level{N}.json correspondiente sirve como ejemplo de formato y
  -- de un nivel jugable. Si la lectura falla (archivo ausente, UTF-8 inválido)
  -- seguimos sin él: el prompt igual lleva el schema completo, solo perdemos el
  -- ejemplo concreto. `getDataFileName` resuelve la ruta empaquetada por Cabal.
  exampleText <- loadExample env profile
  let prompt = promptText profile exampleText
  debugLog env ("prompt: " <> T.unpack prompt)
  -- Primer intento; si falla, un segundo y último intento (la API es estocástica
  -- con temperature alta, así que un reintento suele bastar para superar un JSON
  -- malformado puntual).
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

{- | Lee el few-shot del nivel (@levels/level{index+1}.json@) desde los data
files empaquetados por Cabal, o 'Nothing' si no se puede leer.

Los archivos de nivel son 1-based ('level1.json'..), de ahí el @+ 1@. Una falla
de lectura no es fatal: degradamos a 'Nothing' (con una traza de debug) y el
prompt sigue valiendo gracias al schema embebido.
-}
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

{- | Un único intento de generación: request HTTP + validación.

Devuelve 'Just' solo si la API respondió 2xx y el cuerpo produjo una
'LevelDefinition' que pasa /tanto/ 'decodeLevelDefinition' /como/ 'buildWorld'.
Toda excepción se atrapa con @try \@SomeException@ y se traduce a 'Nothing', de
modo que el llamador ('generateOne') decide si reintenta.
-}
attempt :: GeneratorEnv -> Text -> IO (Maybe LevelDefinition)
attempt env prompt = do
  -- `parseRequest` falla en `IO` si la URL es inválida; al ser una constante del
  -- código no debería ocurrir, pero lo cubrimos con el mismo `try` que la llamada.
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
            , -- La API key viaja en el header `x-api-key`. `redactHeaders` hace que
              -- el `Show` del `Request` (que puede aparecer dentro de una
              -- `HttpException` y terminar en stderr vía `show err`) lo enmascare,
              -- evitando filtrar el secreto en los logs de error.
              redactHeaders = Set.fromList ["x-api-key"]
            }
    httpLbs req (geManager env)
  case result of
    Left err -> do
      warn ("falla de red: " <> show err)
      pure Nothing
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
          interpretBody env (responseBody resp)
      | otherwise -> do
          warn ("status inesperado: " <> show (statusCode (responseStatus resp)))
          pure Nothing
 where
  -- Cuerpo JSON del request. `max_tokens` holgado (un nivel completo es grande) y
  -- `temperature` alta para que cada corrida produzca un layout distinto. `prompt`
  -- se captura del scope de `attempt` (igual que en `Adapters.BehaviourResolver`).
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

{- | Parsea el cuerpo de la respuesta, extrae el JSON del nivel y lo valida.

Decodifica la respuesta de la Messages API (tolerante a bloques sin texto), toma
el primer bloque @type == "text"@, le saca los code fences si vienen y pasa el
texto por el pipeline puro de carga ('decodeLevelDefinition' + 'buildWorld'). Si
ambos dan 'Right' devuelve 'Just'; cualquier otra cosa es 'Nothing' (con traza de
debug del motivo) para que 'generateOne' decida si reintenta.
-}
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
          -- Reutilizamos el MISMO pipeline puro que la carga de niveles fijos:
          -- decode estructural seguido del build con todas sus validaciones
          -- (ids únicos, conteo de jefes, arena coherente, etc.). Así un nivel
          -- generado que pasa es tan válido como uno autoral.
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

-- ---------------------------------------------------------------------------
-- Helpers de presentación y normalización de texto
-- ---------------------------------------------------------------------------

-- | Warning de fallback a 'stderr' (prefijo @[level-generator]@).
warn :: String -> IO ()
warn msg =
  hPutStrLn stderr ("[level-generator] " <> msg <> "; uso nivel fijo.")

preview :: Text -> String
preview = T.unpack . T.take 600

{- | Preview legible (UTF-8 tolerante, truncada) del cuerpo crudo de la
respuesta, para las trazas de debug.
-}
previewBody :: BL.ByteString -> String
previewBody = preview . decodeUtf8Lenient . BL.toStrict

{- | Quita los code fences de Markdown (@```json ... ```@ o @``` ... ```@) si el
modelo los agregó pese a que el prompt pide JSON crudo.

Tolera el caso común: el modelo envuelve la respuesta en un bloque de código.
Quita la línea de apertura (con o sin etiqueta de lenguaje) y el cierre,
quedándose con el JSON interior. Si no hay fences, devuelve el texto tal cual
(recortado de espacios). Es defensivo: 'decodeLevelDefinition' igual fallaría con
fences, así que esto solo evita un reintento innecesario.
-}
stripCodeFences :: Text -> Text
stripCodeFences raw =
  let trimmed = T.strip raw
   in if "```" `T.isPrefixOf` trimmed
        then
          -- Saltea la primera línea (``` o ```json) y elimina el cierre ```.
          let afterOpen = T.drop 1 (T.dropWhile (/= '\n') trimmed)
              withoutClose = fst (T.breakOn "```" afterOpen)
           in T.strip withoutClose
        else trimmed

{- | Extrae el texto del primer bloque @type == "text"@ de la respuesta.

Trabaja en la mónada 'Maybe': 'find' toma el primer bloque con @type == "text"@
(ignorando @thinking@, @tool_use@ u otros que no traen texto) y desempaqueta su
@text@ opcional. 'Nothing' si no hay ningún bloque de texto.
-}
firstText :: AnthropicResponse -> Maybe Text
firstText resp = do
  block <- find ((== "text") . acType) (arContent resp)
  acText block

-- ---------------------------------------------------------------------------
-- Diseño del prompt
-- ---------------------------------------------------------------------------

{- | Prompt para el generador de niveles: instruye al modelo a devolver __solo__
un objeto JSON válido del schema de nivel (sin markdown ni explicación) e incluye
el schema, las reglas según el rol, el few-shot (con la consigna explícita de NO
copiarlo) y el tema opcional.

El few-shot es opcional: si no se pudo leer el ejemplo ('Nothing'), el prompt
omite esa sección pero conserva el schema completo, suficiente para que el modelo
genere un nivel estructuralmente válido.
-}
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
  -- Encabezado: rol del modelo y la regla dura de salida (solo JSON).
  intro =
    "Sos un diseñador de niveles para un plataformero 2D estilo Wonder Boy. "
      <> "Generá UN nivel jugable y devolvé EXCLUSIVAMENTE un objeto JSON válido "
      <> "que cumpla el schema de abajo. NO incluyas markdown, NO uses bloques de "
      <> "código, NO agregues explicaciones ni texto fuera del JSON: tu respuesta "
      <> "completa debe ser el objeto JSON y nada más."

  -- Descripción del schema, derivada de Domain.Model.LevelDefinition.
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

  -- Reglas de jugabilidad transversales a todos los roles. Las reglas de "exit
  -- apoyado", "piso continuo" y "paredes solo en los extremos" son defensivas
  -- contra dos artefactos observados en niveles generados: (1) el exit dibujado
  -- flotando cuando el modelo lo ubicaba sin piso debajo, y (2) "paredes
  -- invisibles" que encierran al jugador, porque el render oculta toda columna
  -- alta y angosta apoyada en el piso salvo la del borde derecho del mapa (ver
  -- 'platformKind'/'renderPlatform' en 'Adapters.Gloss.Rendering'). Pedir piso
  -- continuo, exit apoyado y paredes solo en los extremos reduce ambos casos.
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

  -- Cierre: reitera la consigna de variedad y de salida limpia.
  closing =
    "Generá AHORA el objeto JSON del nivel, distinto del ejemplo, respetando "
      <> "todas las reglas. Recordá: la respuesta debe ser SOLO el JSON."

{- | Sección de reglas de contenido específica del rol del nivel.

Cada rol fija qué elementos del schema debe (y no debe) usar el nivel, alineado
con la semántica de 'LevelRole' documentada en el puerto.
-}
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

{- | Sección temática opcional del prompt.

Devuelve una lista vacía cuando el usuario no pidió tema ('Nothing'), de modo que
'promptText' no menciona ningún tema; con 'Just t' agrega una instrucción para
aplicar esa directiva al diseño.
-}
themeSection :: Maybe Text -> [Text]
themeSection Nothing = []
themeSection (Just theme) =
  [ "Tema solicitado por el usuario: aplicá esta directiva temática al diseño del "
      <> "nivel (nombres, disposición y atmósfera implícita): "
      <> theme
  ]

{- | Sección del few-shot opcional del prompt.

Devuelve una lista vacía cuando no hay ejemplo disponible ('Nothing'); con 'Just'
adjunta el JSON del nivel de ejemplo y la consigna EXPLÍCITA de tratarlo como
muestra de formato y jugabilidad, no como algo a copiar.
-}
exampleSection :: Maybe Text -> [Text]
exampleSection Nothing = []
exampleSection (Just example) =
  [ "Este es un ejemplo de formato y de un nivel jugable. Es SOLO una referencia "
      <> "de estructura y rangos de coordenadas: NO lo copies, generá un layout "
      <> "DISTINTO (otras plataformas, otros enemigos, otras posiciones).\n\n"
      <> example
  ]

-- ---------------------------------------------------------------------------
-- Tipos de la respuesta de la API (FromJSON parcial, solo lo que usamos)
-- ---------------------------------------------------------------------------

{- | Vista mínima de la respuesta de la Messages API: solo el array @content@.

Modelamos únicamente los campos que consumimos; el resto del JSON se ignora.
Idéntico al tipo homónimo de 'Adapters.BehaviourResolver'.
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
