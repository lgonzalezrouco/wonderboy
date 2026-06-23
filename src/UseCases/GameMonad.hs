{- | Pila monádica central del motor.

'GameM' es el contexto en el que corre toda la lógica de 'UseCases/'.
Combina tres efectos apilados sobre una base pura:

  * 'MonadReader' 'GameConfig' — acceso de solo lectura a la configuración global.
  * 'MonadState'  'GameState'  — estado mutable del juego ('World').
  * 'MonadError'  'GameError'  — manejo de errores recuperables sin lanzar excepciones.

Nada en este módulo es 'IO'. Toda la impureza vive en @Adapters/@ y @Frameworks/@.
Ver @docs\/adr\/0008-gamemonad-stack.md@ para la justificación de cada capa y su orden.
-}
module UseCases.GameMonad (
  -- * Configuración
  GameConfig (..),
  defaultConfig,
  configForLevelCatalog,
  physicsParamsFromConfig,
  lifeParamsFromConfig,
  combatParamsFromConfig,
  throwParamsFromConfig,

  -- * Errores
  GameError (..),

  -- * Estado
  GameState (..),
  initialGameState,
  startLevel,
  advanceAfterLevelComplete,
  restartRun,
  gameViewFromState,

  -- * La mónada
  GameM (..),
  runGameM,
)
where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)

-- Grupo 2 — terceros (mtl)
-- `mtl` provee transformadores de mónadas y sus typeclasses.
-- Cada import es explícito para documentar exactamente qué usamos de cada módulo.
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)

-- Grupo 3 — proyecto

import Domain.Logic.BossArena (bossArenaSealed, playerWithinBossArena)
import Domain.Logic.LevelFlow (findLivingBoss, showBossExitHint, showExitScoreHint)
import Domain.Model.Enemy (enemyHealth, enemyMaxHealth)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.GameView (GameView (..))
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.BossHealth (BossHealth, bossHealth)
import Domain.ValueObjects.CombatParams (CombatParams (..), combatParams)
import Domain.ValueObjects.Damage (Damage, damage)
import Domain.ValueObjects.Frames (Frames, frames)
import Domain.ValueObjects.Health (Health)
import Domain.ValueObjects.LevelCount (LevelCount, levelCount)
import Domain.ValueObjects.LifeParams (LifeParams (..), lifeParams)
import Domain.ValueObjects.Lives (Lives, lives)
import Domain.ValueObjects.PhysicsParams (PhysicsParams, physicsParams)
import Domain.ValueObjects.Score (Score, score)
import Domain.ValueObjects.ThrowParams (ThrowParams (..), throwParams)

-- ---------------------------------------------------------------------------
-- Tipos de la pila
-- ---------------------------------------------------------------------------

{- | Configuración global del juego, inmutable durante una partida.

Todos los parámetros que no cambian frame a frame viven aquí:
el 'ReaderT' los pone a disposición de cualquier acción en 'GameM' vía 'ask'\/'asks'.

__Por qué `data` y no `newtype`?__ — El `newtype` requiere exactamente un campo.
'GameConfig' tiene varios campos, así que `data` es la opción correcta.
-}
data GameConfig = GameConfig
  { gcGravity :: Float
  -- ^ Aceleración gravitatoria en px\/s² (hacia abajo).
  --   Se aplica sobre la componente vy del jugador en cada frame (M3).
  --   Valor típico para un juego de plataformas en píxeles: 800–1200 px\/s².
  , gcMoveSpeed :: Float
  -- ^ Velocidad horizontal del jugador al recibir input (px\/s).
  --   @Domain.Logic.Step@ la recibe vía 'PhysicsParams'.
  , gcJumpSpeed :: Float
  -- ^ Velocidad vertical inicial al saltar desde el suelo (px\/s).
  , gcStartingLives :: Lives
  -- ^ Vidas al iniciar una partida nueva (run-wide; no por nivel).
  , gcMaxHealth :: Health
  -- ^ Salud tras spawn o respawn.
  , gcDeathMargin :: Float
  -- ^ Margen bajo la plataforma más baja para out-of-bounds (px).
  , gcAttackDuration :: Frames
  -- ^ Frames de ventana activa de melee (M10).
  , gcInvincibilityDuration :: Frames
  -- ^ Frames de invencibilidad tras contacto enemigo o respawn (M10).
  , gcContactDamage :: Damage
  -- ^ Daño por frame de contacto enemigo (M10).
  , gcMeleeReach :: Float
  -- ^ Alcance horizontal del melee en px lógicos (M10).
  , gcMeleeDamage :: Damage
  -- ^ Daño infligido a un enemigo por un melee que conecta (M10).
  , gcLevelCount :: LevelCount
  -- ^ Niveles en el run actual; la victoria ocurre al completar el último.
  , gcThrowCooldown :: Frames
  -- ^ Frames de espera tras despawn del proyectil del jugador (M19).
  , gcThrowLifetime :: Frames
  -- ^ Vida inicial de cada proyectil lanzado (M19).
  , gcThrowHorizontalSpeed :: Float
  -- ^ Velocidad horizontal de lanzamiento (px/s) (M19).
  , gcThrowLiftSpeed :: Float
  -- ^ Impulso vertical inicial del arco (px/s) (M19).
  , gcProjectileWidth :: Float
  -- ^ Ancho de la caja del proyectil (M19).
  , gcProjectileHeight :: Float
  -- ^ Alto de la caja del proyectil (M19).
  }
  deriving (Eq, Show, Generic)

{- | Configuración por defecto para pruebas y el demo de @app\/Main.hs@.

En Milestone 8 se cargará desde JSON (Aeson) según el nivel.
-}
defaultConfig :: GameConfig
defaultConfig =
  GameConfig
    { gcGravity = 980.0 -- aprox. 1g a escala de píxeles (px/s²)
    , gcMoveSpeed = 200.0 -- 200 px/s de movimiento horizontal
    , gcJumpSpeed = 400.0 -- impulso de salto (px/s)
    , gcStartingLives = lives 3
    , gcMaxHealth = defaultMaxHealth
    , gcDeathMargin = 64.0
    , gcAttackDuration = frames 6
    , gcInvincibilityDuration = frames 60
    , gcContactDamage = damage 1
    , gcMeleeReach = 15.0
    , gcMeleeDamage = damage 1
    , gcLevelCount = levelCount 3
    , gcThrowCooldown = frames 30
    , gcThrowLifetime = frames 120
    , gcThrowHorizontalSpeed = 280.0
    , gcThrowLiftSpeed = 320.0
    , gcProjectileWidth = 12.0
    , gcProjectileHeight = 12.0
    }

-- | Ajusta 'gcLevelCount' al tamaño del catálogo de niveles del run.
configForLevelCatalog :: [a] -> GameConfig
configForLevelCatalog paths =
  defaultConfig{gcLevelCount = levelCount (length paths)}

-- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Step.step'.
physicsParamsFromConfig :: GameConfig -> PhysicsParams
physicsParamsFromConfig cfg =
  physicsParams
    (gcGravity cfg)
    (gcMoveSpeed cfg)
    (gcJumpSpeed cfg)

{- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.PlayerLife'.

Los frames de invencibilidad de respawn usan 'gcInvincibilityDuration', el __mismo__ campo
que los de contacto (ver 'combatParamsFromConfig'): hoy comparten valor a propósito.
Si en el futuro hace falta tunearlos por separado, se añade un campo dedicado a 'GameConfig'.
-}
lifeParamsFromConfig :: GameConfig -> LifeParams
lifeParamsFromConfig cfg =
  lifeParams
    (gcMaxHealth cfg)
    (gcDeathMargin cfg)
    (gcInvincibilityDuration cfg)

{- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Combat'.

Los frames de invencibilidad de contacto usan 'gcInvincibilityDuration', el mismo campo
que los de respawn (ver 'lifeParamsFromConfig'); el acoplamiento es intencional por ahora.
-}
combatParamsFromConfig :: GameConfig -> CombatParams
combatParamsFromConfig cfg =
  combatParams
    (gcAttackDuration cfg)
    (gcInvincibilityDuration cfg)
    (gcContactDamage cfg)
    (gcMeleeReach cfg)
    (gcMeleeDamage cfg)

-- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Projectiles'.
throwParamsFromConfig :: GameConfig -> ThrowParams
throwParamsFromConfig cfg =
  throwParams
    (gcThrowCooldown cfg)
    (gcThrowLifetime cfg)
    (gcThrowHorizontalSpeed cfg)
    (gcThrowLiftSpeed cfg)
    (gcProjectileWidth cfg)
    (gcProjectileHeight cfg)
    (gcMeleeDamage cfg)

{- | Errores recuperables del motor.

__Por qué seguimos con `newtype String` y no un tipo suma?__

En Milestone 2 ninguna operación puede fallar: los modelos son records simples
sin invariantes complejos. Introducir un tipo suma (@OutOfBounds@, @InvalidInput@, …)
ahora sería especulativo — no sabemos exactamente qué errores reales surgirán hasta
tener física y colisiones (M3). El tipo suma vendrá en M3 cuando aparezca el primer
error real y podamos diseñarlo con los casos concretos necesarios.
-}
newtype GameError = GameError String
  deriving (Eq, Show, Generic)

{- | Estado mutable del juego: mundo de nivel + estado run-wide.

Contiene el 'World' del nivel actual más vidas y fase de la partida.
Ver @docs\/adr\/0012-gamestate-run-snapshot.md@.
-}
data GameState = GameState
  { gsWorld :: World
  , gsLives :: Lives
  , gsPhase :: GamePhase
  , gsScore :: Score
  -- ^ Puntuación del nivel actual; se reinicia al cargar un nivel (M18).
  , gsLevelIndex :: Int
  -- ^ Posición 1-based del nivel actual dentro del run.
  }
  deriving (Eq, Show, Generic)

-- | Estado inicial de una partida nueva a partir de un mundo de nivel.
initialGameState :: GameConfig -> World -> GameState
initialGameState cfg = startLevel cfg (gcStartingLives cfg) 1

-- | Carga un nivel en el run conservando vidas y reiniciando puntuación y salud.
startLevel :: GameConfig -> Lives -> Int -> World -> GameState
startLevel cfg runLives levelIndex w =
  GameState
    { gsWorld = w{worldPlayer = spawnPlayer (gcMaxHealth cfg) (worldSpawnPoint w)}
    , gsLives = runLives
    , gsPhase = Playing
    , gsScore = score 0
    , gsLevelIndex = levelIndex
    }

-- | Avanza al siguiente nivel tras confirmar 'LevelComplete'.
advanceAfterLevelComplete :: GameConfig -> GameState -> World -> GameState
advanceAfterLevelComplete cfg gs =
  startLevel cfg (gsLives gs) (gsLevelIndex gs + 1)

-- | Reinicia el run desde el nivel 1 con vidas iniciales.
restartRun :: GameConfig -> World -> GameState
restartRun cfg = startLevel cfg (gcStartingLives cfg) 1

{- | Proyección para el adaptador de renderizado (sin importar 'GameMonad' desde Adapters).

Recibe 'GameConfig' para que el HUD derive sus máximos (salud, vidas iniciales) de la
configuración y no de constantes duplicadas en el adaptador.
-}
gameViewFromState :: GameConfig -> GameState -> GameView
gameViewFromState cfg gs =
  let w = gsWorld gs
      s = gsScore gs
   in GameView
        { gvWorld = w
        , gvLives = gsLives gs
        , gvPhase = gsPhase gs
        , gvMaxHealth = gcMaxHealth cfg
        , gvStartingLives = gcStartingLives cfg
        , gvScore = s
        , gvBossHealth = bossHealthFromWorld w
        , gvCombatParams = combatParamsFromConfig cfg
        , gvLevelIndex = gsLevelIndex gs
        , gvExitScoreHint =
            if showExitScoreHint s w
              then Just (s, worldMinScore w)
              else Nothing
        , gvBossExitHint = showBossExitHint s w
        , gvBossArenaSealed = bossArenaSealed w
        }

-- | Proyecta salud del jefe vivo para el HUD (como máximo un jefe por nivel).
bossHealthFromWorld :: World -> Maybe BossHealth
bossHealthFromWorld w
  | not (playerWithinBossArena w) = Nothing
  | otherwise = do
      e <- findLivingBoss w
      pure (bossHealth (enemyHealth e) (enemyMaxHealth e))

-- ---------------------------------------------------------------------------
-- La mónada GameM
-- ---------------------------------------------------------------------------

{- | Mónada del motor: @ReaderT GameConfig (StateT GameState (ExceptT GameError Identity))@.

La pila se lee de afuera hacia adentro. Cada capa agrega un efecto:

@
  Identity                        ← base pura (sin efectos; no es un transformer sino el fondo)
  ExceptT GameError Identity       ← agrega manejo de errores sobre Identity
  StateT  GameState (ExceptT ...)  ← agrega estado mutable sobre la capa anterior
  ReaderT GameConfig (StateT ...)  ← agrega configuración de solo lectura encima de todo
@

Al ejecutar ('runGameM'), se desenvuelven de afuera hacia adentro:
primero el Reader (se suministra el Config), luego el State (se da el estado inicial),
luego el ExceptT (se "abre" la posibilidad de error), luego Identity (extrae el valor puro).

Por qué `newtype` y no un alias de tipo:

  * El alias sería @type GameM a = ReaderT ... a@ — GHC lo expandiría en cada
    mensaje de error, haciendo los tipos ilegibles.
  * El `newtype` le da un nombre corto ('GameM') a algo complejo, y permite
    agregar instancias propias en el futuro si hiciera falta.
-}
newtype GameM a = GameM
  { -- `unGameM` desempaqueta el newtype para operar sobre la pila interna (por ejemplo, en `runGameM`).
    unGameM ::
      ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a
  }
  deriving
    ( -- | Permite aplicar una función al resultado: `fmap (+1) :: GameM Int -> GameM Int`.
      Functor
    , -- | Permite `pure :: a -> GameM a` (envuelve un valor puro) y `<*>` (aplicación en contexto).
      Applicative
    , -- | Permite `>>=` (bind) para secuenciar acciones: `accionA >>= \resultado -> accionB resultado`.
      Monad
    , -- | Habilita `ask :: GameM GameConfig` (leer la config completa) y
      --   `asks f :: GameM b` (leer un campo: `asks gcGravity`).
      MonadReader GameConfig
    , -- | Habilita `get :: GameM GameState`, `put :: GameState -> GameM ()`,
      --   y `modify :: (GameState -> GameState) -> GameM ()`.
      MonadState GameState
    , -- | Habilita `throwError :: GameError -> GameM a` y
      --   `catchError :: GameM a -> (GameError -> GameM a) -> GameM a`.
      MonadError GameError
    )

-- ---------------------------------------------------------------------------
-- Intérprete / runner
-- ---------------------------------------------------------------------------

{- | Ejecuta una acción en 'GameM' y devuelve el resultado o un error.

Cada @run*@ desenvuelve (pela) una capa del transformer stack,
suministrando los valores necesarios para eliminar ese efecto:

@
  unGameM action
    :: ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a

  flip runReaderT cfg
    :: StateT GameState (ExceptT GameError Identity) a
       (suministramos la configuración; el Reader desaparece)

  flip runStateT st
    :: ExceptT GameError Identity (a, GameState)
       (suministramos el estado inicial; el State devuelve (resultado, estado_final))

  runExceptT
    :: Identity (Either GameError (a, GameState))
       (el ExceptT se convierte en un Either; los errores quedan capturados)

  runIdentity
    :: Either GameError (a, GameState)
       (quitamos la envoltura Identity; queda el valor puro)
@

Usamos `flip` porque `runReaderT :: ReaderT r m a -> r -> m a` toma
primero el transformer y luego el entorno. `flip runReaderT cfg` invierte
el orden, dejando el transformer como último argumento para componer con `.`.

Ejemplo de uso (ver también @app\/Main.hs@):

@
runGameM defaultConfig initialWorld someAction
  -- :: Either GameError (a, World)
@
-}
runGameM ::
  GameConfig ->
  GameState ->
  GameM a ->
  Either GameError (a, GameState)
runGameM cfg st =
  runIdentity . runExceptT . flip runStateT st . flip runReaderT cfg . unGameM
