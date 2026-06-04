{- | Pila monádica central del motor.

'GameM' es el contexto en el que corre toda la lógica de 'UseCases/'.
Combina tres efectos apilados sobre una base pura:

  * 'MonadReader' 'GameConfig' — acceso de solo lectura a la configuración global.
  * 'MonadState'  'GameState'  — estado mutable del juego ('World').
  * 'MonadError'  'GameError'  — manejo de errores recuperables sin lanzar excepciones.

Nada en este módulo es 'IO'. Toda la impureza vive en @Adapters/@ y @Frameworks/@.
Ver @docs\/gamemonad.md@ para la justificación de cada capa y su orden.
-}
module UseCases.GameMonad (
  -- * Configuración
  GameConfig (..),
  defaultConfig,
  physicsParamsFromConfig,

  -- * Errores
  GameError (..),

  -- * Estado
  GameState,

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
import Domain.Model.World (World)
import Domain.ValueObjects.PhysicsParams (PhysicsParams, physicsParams)

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
    }

-- | Proyecta 'GameConfig' al value object puro usado por 'Domain.Logic.Step.step'.
physicsParamsFromConfig :: GameConfig -> PhysicsParams
physicsParamsFromConfig cfg =
  physicsParams
    (gcGravity cfg)
    (gcMoveSpeed cfg)
    (gcJumpSpeed cfg)

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

{- | Estado mutable del juego: el 'World' completo.

Cambio respecto a Milestone 1: antes era @()@ (unit, sin datos).
Ahora contiene el estado real de la simulación que @UpdateGame@ lee y modifica.

Usamos `type` (alias de tipo) en lugar de `newtype` porque no necesitamos un
tipo nominativo distinto — el alias es suficiente para renombrar 'World' a
'GameState' en la firma de 'runGameM' sin overhead extra.
-}
type GameState = World

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
