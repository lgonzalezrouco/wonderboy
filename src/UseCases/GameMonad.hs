module UseCases.GameMonad (
  GameConfig (..),
  defaultConfig,
  configForLevelCatalog,
  physicsParamsFromConfig,
  lifeParamsFromConfig,
  combatParamsFromConfig,
  throwParamsFromConfig,
  GameError (..),
  GameState (..),
  initialGameState,
  startLevel,
  advanceAfterLevelComplete,
  restartRun,
  GameView (..),
  gameViewFromState,
  bossHealthFromWorld,
  GameM (..),
  runGameM,
)
where

import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)

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

newtype GameError = GameError String
  deriving (Eq, Show, Generic)

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

runGameM ::
  GameConfig ->
  GameState ->
  GameM a ->
  Either GameError (a, GameState)
runGameM cfg st =
  runIdentity . runExceptT . flip runStateT st . flip runReaderT cfg . unGameM
