{- | Fachada del puerto primario del motor.

Re-exporta 'GameConfig', 'GameState' y 'GameView' para que los consumidores
(@Frameworks/@, @Adapters/@, tests) no necesiten cambiar sus imports.

'GameM' es el contexto en el que corre toda la lógica de 'UseCases/'.
Combina tres efectos apilados sobre una base pura:

  * 'MonadReader' 'GameConfig' — acceso de solo lectura a la configuración global.
  * 'MonadState'  'GameState'  — estado mutable del juego ('World').
  * 'MonadError'  'GameError'  — manejo de errores recuperables sin lanzar excepciones.

Nada en este módulo es 'IO'. Toda la impureza vive en @Adapters/@ y @Frameworks/@.
Ver @docs\/adr\/0008-gamemonad-stack.md@ para la justificación de cada capa y su orden.
-}
module UseCases.GameMonad (
  -- * Configuración (re-exportada de UseCases.Engine.GameConfig)
  GameConfig (..),
  defaultConfig,
  configForLevelCatalog,
  physicsParamsFromConfig,
  lifeParamsFromConfig,
  combatParamsFromConfig,
  throwParamsFromConfig,

  -- * Errores
  GameError (..),

  -- * Estado (re-exportado de UseCases.Engine.GameState)
  GameState (..),
  initialGameState,
  startLevel,
  advanceAfterLevelComplete,
  restartRun,

  -- * Vista (re-exportada de UseCases.Engine.GameView)
  GameView (..),
  gameViewFromState,
  bossHealthFromWorld,

  -- * La mónada
  GameM (..),
  runGameM,
)
where

-- Grupo 1 — stdlib / base
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)

-- Grupo 2 — terceros (mtl)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)

-- Grupo 3 — proyecto (re-exportaciones)
import UseCases.Engine.GameConfig (
  GameConfig (..),
  combatParamsFromConfig,
  configForLevelCatalog,
  defaultConfig,
  lifeParamsFromConfig,
  physicsParamsFromConfig,
  throwParamsFromConfig,
 )
import UseCases.Engine.GameState (
  GameState (..),
  advanceAfterLevelComplete,
  initialGameState,
  restartRun,
  startLevel,
 )
import UseCases.Engine.GameView (
  GameView (..),
  bossHealthFromWorld,
  gameViewFromState,
 )

-- ---------------------------------------------------------------------------
-- Errores
-- ---------------------------------------------------------------------------

{- | Errores recuperables del motor.

Newtype sobre 'String'; un tipo suma con variantes concretas puede agregarse
cuando aparezcan errores distinguibles que el motor trate de forma diferente.
-}
newtype GameError = GameError String
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- La mónada GameM
-- ---------------------------------------------------------------------------

{- | Mónada del motor: @ReaderT GameConfig (StateT GameState (ExceptT GameError Identity))@.

La pila se lee de afuera hacia adentro. Cada capa agrega un efecto:

@
  Identity                        ← base pura
  ExceptT GameError Identity       ← manejo de errores
  StateT  GameState (ExceptT ...)  ← estado mutable
  ReaderT GameConfig (StateT ...)  ← configuración de solo lectura
@
-}
newtype GameM a = GameM
  { unGameM ::
      ReaderT GameConfig (StateT GameState (ExceptT GameError Identity)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader GameConfig
    , MonadState GameState
    , MonadError GameError
    )

-- ---------------------------------------------------------------------------
-- Intérprete / runner
-- ---------------------------------------------------------------------------

{- | Ejecuta una acción en 'GameM' y devuelve el resultado o un error.

Desenvuelve la pila transformer de afuera hacia adentro:
reader → state → except → identity.
-}
runGameM ::
  GameConfig ->
  GameState ->
  GameM a ->
  Either GameError (a, GameState)
runGameM cfg st =
  runIdentity . runExceptT . flip runStateT st . flip runReaderT cfg . unGameM
