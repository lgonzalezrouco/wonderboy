{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Adaptador de 'LevelContentPort' sobre la API de Anthropic.

Un actor (Anthropic LLM) = un adapter. Absorbe la lógica de los adaptadores
anteriores (@Adapters.BehaviourResolver@ y @Adapters.LevelGenerator@) en una
sola implementación de 'LevelContentPort'. Ver @docs\/adr\/0019-level-content-port.md@.

'AnthropicContent' (@ReaderT AnthropicEnv IO@) encapsula el monad stack de 'IO';
'runAnthropicContent' es el único punto de entrada desde el driver IO del arranque
(@Adapters.BootstrapRunIO@).
-}
module Adapters.Anthropic.Content (
  AnthropicEnv (..),
  AnthropicContent,
  runAnthropicContent,

  -- * Helpers exportados para tests
  ResolverReply (..),
  extractJsonObject,
  resolvedFromReply,
)
where

-- Grupo 1 — stdlib / base
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.IO (hPutStrLn, stderr)

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

-- Grupo 2 — terceros
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (
  FromJSON (..),
  Value,
  decode,
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )

-- Grupo 3 — proyecto
import Adapters.Anthropic.Client (
  AnthropicClient (..),
  FeatureCfg (..),
  callAnthropic,
  debugLog,
  previewText,
 )
import Domain.Logic.BuildWorld (buildWorld)
import Domain.Model.EnemyKind (EnemyKind)
import Domain.Model.LevelDefinition (
  LevelBuildError (..),
  LevelDefinition,
  ResolvedBehaviour (..),
  parseBehaviourArchetype,
 )
import Domain.ValueObjects.Amplifier (identityAmplifier, mkAmplifier, unAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Multiplier (identityMultiplier, mkMultiplier, unMultiplier)
import UseCases.Ports.LevelContentPort (
  LevelContentPort (..),
  LevelProfile (..),
  LevelRole (..),
 )
import UseCases.Serialization.LevelCodec (
  decodeLevelText,
  encodeLevelDefinitionText,
 )

-- ---------------------------------------------------------------------------
-- Entorno y monad
-- ---------------------------------------------------------------------------

{- | Entorno compartido entre el generador y el resolver.

Un único 'AnthropicClient' (manager + key + URL) y configuraciones separadas
para cada feature, permitiendo distintos modelos y timeouts.
-}
data AnthropicEnv = AnthropicEnv
  { aeClient :: AnthropicClient
  -- ^ Conexión TLS reusable.
  , aeGeneratorCfg :: FeatureCfg
  -- ^ Modelo, timeout y debug del generador de niveles.
  , aeResolverCfg :: FeatureCfg
  -- ^ Modelo, timeout y debug del resolver de arquetipos.
  }

-- | Adapter de 'LevelContentPort' con acceso a 'IO' vía @ReaderT AnthropicEnv@.
newtype AnthropicContent a = AnthropicContent
  {unAnthropicContent :: ReaderT AnthropicEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AnthropicEnv)

-- | Ejecuta el adapter con el entorno dado.
runAnthropicContent :: AnthropicEnv -> AnthropicContent a -> IO a
runAnthropicContent env m = runReaderT (unAnthropicContent m) env

-- ---------------------------------------------------------------------------
-- Instancia LevelContentPort
-- ---------------------------------------------------------------------------

instance LevelContentPort AnthropicContent where
  generateLevel profile = do
    env <- ask
    liftIO (generateOne env profile)

  resolveBehaviourHint kind hint = do
    env <- ask
    liftIO (resolveOne env kind hint)

  -- Los slots son independientes: una sola conexión TLS reusable atiende las
  -- llamadas en paralelo, recortando la latencia de arranque a la del slot más
  -- lento en vez de la suma de todos.
  generateLevels profiles = do
    env <- ask
    liftIO (mapConcurrently (generateOne env) profiles)

-- ---------------------------------------------------------------------------
-- Generación de niveles
-- ---------------------------------------------------------------------------

{- | Intentos de generación por slot.

Con @temperature@ 0.9 cada intento produce una muestra distinta, así que
reintentar con el mismo @body@ recupera tanto fallas de red como respuestas que
decodifican mal o no superan el 'buildWorld'.
-}
maxGenerationAttempts :: Int
maxGenerationAttempts = 2

generateOne :: AnthropicEnv -> LevelProfile -> IO (Maybe LevelDefinition)
generateOne env profile = do
  let cfg = aeGeneratorCfg env
      client = aeClient env
      exampleText = encodeLevelDefinitionText <$> profileExample profile
  debugLog (fcDebug cfg) "level-generator" $
    "generando: índice="
      <> show (profileIndex profile)
      <> " rol="
      <> show (profileRole profile)
  let prompt = generatorPromptText profile exampleText
  debugLog (fcDebug cfg) "level-generator" ("prompt: " <> T.unpack prompt)
  let body = generatorBody cfg prompt
      attempt n = do
        mDef <- callAnthropic client cfg body >>= maybe (pure Nothing) (tryDecode cfg)
        case mDef of
          Just def -> pure (Just def)
          Nothing
            | n < maxGenerationAttempts -> do
                debugLog (fcDebug cfg) "level-generator" $
                  "intento " <> show n <> " sin nivel válido; reintento"
                attempt (n + 1)
            | otherwise -> do
                hPutStrLn
                  stderr
                  ( "[level-generator] no se pudo generar el nivel "
                      <> show (profileIndex profile)
                      <> " tras "
                      <> show maxGenerationAttempts
                      <> " intentos; uso nivel fijo."
                  )
                pure Nothing
  attempt 1

{- | Decodifica y __valida__ un nivel generado.

Replica la barrera del generador anterior: además de decodificar el JSON, corre
'buildWorld' y descarta el nivel si no construye, para que un nivel con JSON
válido pero estructura inválida no entre al catálogo (y reviente el juego al
llegar a ese slot). 'Nothing' ⇒ el llamador reintenta o usa el nivel fijo.
-}
tryDecode :: FeatureCfg -> Text -> IO (Maybe LevelDefinition)
tryDecode cfg raw = do
  let levelJson = stripCodeFences raw
  debugLog (fcDebug cfg) "level-generator" ("JSON del nivel extraído: " <> previewText levelJson)
  case decodeLevelText levelJson of
    Left err -> do
      debugLog (fcDebug cfg) "level-generator" ("decode falló: " <> err)
      pure Nothing
    Right def -> case buildWorld def of
      Left (LevelBuildError msg) -> do
        debugLog (fcDebug cfg) "level-generator" ("build falló: " <> T.unpack msg)
        pure Nothing
      Right _ -> do
        debugLog (fcDebug cfg) "level-generator" "nivel válido (decode + build OK)"
        pure (Just def)

generatorBody :: FeatureCfg -> Text -> Value
generatorBody cfg prompt =
  object
    [ "model" .= fcModel cfg
    , "max_tokens" .= (4096 :: Int)
    , "temperature" .= (0.9 :: Double)
    , "messages"
        .= [ object
              [ "role" .= ("user" :: Text)
              , "content" .= prompt
              ]
           ]
    ]

{- | Quita cercas markdown si el modelo las agregó.

Soporta cercas multilínea (@```json\\n…\\n```@) y de una sola línea
(@```{…}```@): en el primer caso descarta la etiqueta de lenguaje hasta el salto
de línea; en el segundo conserva el contenido en la misma línea.
-}
stripCodeFences :: Text -> Text
stripCodeFences raw =
  let trimmed = T.strip raw
   in if "```" `T.isPrefixOf` trimmed
        then
          let afterFence = T.drop 3 trimmed
              afterOpen
                | T.any (== '\n') afterFence = T.drop 1 (T.dropWhile (/= '\n') afterFence)
                | otherwise = afterFence
              withoutClose = fst (T.breakOn "```" afterOpen)
           in T.strip withoutClose
        else trimmed

generatorPromptText :: LevelProfile -> Maybe Text -> Text
generatorPromptText profile mExample =
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

-- ---------------------------------------------------------------------------
-- Resolución de arquetipos
-- ---------------------------------------------------------------------------

resolveOne :: AnthropicEnv -> EnemyKind -> Text -> IO (Maybe ResolvedBehaviour)
resolveOne env kind hint = do
  let cfg = aeResolverCfg env
      client = aeClient env
  debugLog (fcDebug cfg) "behaviour-resolver" $
    "consultando: kind=" <> show kind <> " hint=" <> show hint
  let body = resolverBody cfg kind hint
  mText <- callAnthropic client cfg body
  case mText of
    Nothing -> pure Nothing
    Just raw -> do
      debugLog (fcDebug cfg) "behaviour-resolver" ("respuesta cruda: " <> previewText raw)
      case extractJsonObject raw of
        Nothing -> warn cfg "respuesta sin objeto JSON"
        Just jsonText ->
          case decode @ResolverReply (BL.fromStrict (encodeUtf8 jsonText)) of
            Nothing -> warn cfg ("JSON del modelo no parseable: " <> T.unpack jsonText)
            Just reply ->
              case resolvedFromReply reply of
                Nothing -> warn cfg ("arquetipo no reconocido: " <> T.unpack (rrArchetype reply))
                Just rb -> do
                  debugLog (fcDebug cfg) "behaviour-resolver" $
                    "resuelto: "
                      <> show (rbArchetype rb)
                      <> " speed="
                      <> show (unMultiplier (tuningSpeed (rbTuning rb)))
                      <> " reach="
                      <> show (unAmplifier (tuningReach (rbTuning rb)))
                  pure (Just rb)
 where
  warn cfg msg = do
    hPutStrLn stderr ("[behaviour-resolver] " <> msg <> "; uso arquetipo por defecto.")
    debugLog (fcDebug cfg) "behaviour-resolver" msg
    pure Nothing

resolverBody :: FeatureCfg -> EnemyKind -> Text -> Value
resolverBody cfg kind hint =
  object
    [ "model" .= fcModel cfg
    , "max_tokens" .= (64 :: Int)
    , "temperature" .= (0.0 :: Double)
    , "messages"
        .= [ object
              [ "role" .= ("user" :: Text)
              , "content" .= resolverPromptText kind hint
              ]
           ]
    ]

resolverPromptText :: EnemyKind -> Text -> Text
resolverPromptText kind hint =
  "Sos un diseñador de niveles de un plataformero 2D. Para un enemigo ("
    <> T.pack (show kind)
    <> ") con esta descripción, devolvé SOLO un objeto JSON (sin texto extra) con:\n"
    <> "  \"archetype\": \"patrol\" | \"chase\" | \"guard\" (la forma de moverse),\n"
    <> "  \"speed\": número (1.0 normal, <1 más lento, >1 más rápido),\n"
    <> "  \"reach\": número >= 1.0 (1.0 = alcance base del arquetipo, >1 detecta y persigue más lejos),\n"
    <> "  \"toughness\": número >= 1.0 (1.0 = vida base, >1 más resistente),\n"
    <> "Descripción: "
    <> hint

{- | Primer objeto JSON @{@ … @}@ balanceado en un texto con prosa.

Toma desde la primera @{@ hasta su @}@ de cierre (la que vuelve la profundidad de
llaves a cero), de modo que la prosa posterior con más @}@ no extiende el recorte
hasta el final del texto. 'Nothing' si no hay @{@ o el objeto no cierra.
-}
extractJsonObject :: Text -> Maybe Text
extractJsonObject t =
  let afterOpen = T.dropWhile (/= '{') t
   in (`T.take` afterOpen) <$> balancedEnd afterOpen

-- | Largo del primer objeto @{…}@ balanceado (incluye ambas llaves), o 'Nothing'.
balancedEnd :: Text -> Maybe Int
balancedEnd = go (0 :: Int) (0 :: Int) . T.unpack
 where
  go _ _ [] = Nothing
  go depth i (c : cs) =
    let depth' = case c of
          '{' -> depth + 1
          '}' -> depth - 1
          _ -> depth
     in if depth' == 0 && c == '}'
          then Just (i + 1)
          else go depth' (i + 1) cs

-- | Respuesta cruda del resolver de comportamiento (JSON del modelo).
data ResolverReply = ResolverReply
  { rrArchetype :: Text
  -- ^ Arquetipo de movimiento sugerido (@patrol@\/@chase@\/@guard@).
  , rrSpeed :: Maybe Double
  -- ^ Multiplicador de velocidad opcional (1.0 = base).
  , rrReach :: Maybe Double
  -- ^ Amplificador de alcance opcional (>= 1.0).
  , rrToughness :: Maybe Double
  -- ^ Amplificador de resistencia opcional (>= 1.0).
  }

instance FromJSON ResolverReply where
  parseJSON =
    withObject "ResolverReply" $ \o ->
      ResolverReply
        <$> o .: "archetype"
        <*> o .:? "speed"
        <*> o .:? "reach"
        <*> o .:? "toughness"

-- | Mapeo puro de 'ResolverReply' a 'ResolvedBehaviour'.
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
