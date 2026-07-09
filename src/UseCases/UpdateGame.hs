module UseCases.UpdateGame (
  updateGame,
  runFrames,
)
where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, get, modify)

import Domain.Logic.Frame (
  FrameParams (..),
  FrameResult (..),
  PlayingFrame (..),
  advanceSimulationFrame,
 )
import Domain.Model.GamePhase (isSimulationFrozen)
import Domain.ValueObjects.DeltaTime (DeltaTime, isFrozen)
import Domain.ValueObjects.Input (Input)
import UseCases.GameMonad (
  GameConfig (..),
  GameError,
  GameState (..),
  combatParamsFromConfig,
  lifeParamsFromConfig,
  physicsParamsFromConfig,
  runGameM,
  throwParamsFromConfig,
 )

updateGame ::
  (MonadReader GameConfig m, MonadState GameState m) =>
  DeltaTime ->
  Input ->
  m ()
updateGame dt input = do
  st <- get
  unless (isSimulationFrozen (gsPhase st) || isFrozen dt) $ do
    cfg <- ask
    let fp =
          FrameParams
            { fpPhysics = physicsParamsFromConfig cfg
            , fpLife = lifeParamsFromConfig cfg
            , fpCombat = combatParamsFromConfig cfg
            , fpThrow = throwParamsFromConfig cfg
            }
        playing =
          PlayingFrame
            { pfWorld = gsWorld st
            , pfLives = gsLives st
            , pfScore = gsScore st
            , pfLevelIndex = gsLevelIndex st
            }
        result = advanceSimulationFrame fp (gcLevelCount cfg) dt input playing
    modify
      ( \s ->
          s
            { gsWorld = frWorld result
            , gsLives = frLives result
            , gsPhase = frPhase result
            , gsScore = frScore result
            }
      )

runFrames ::
  GameConfig ->
  Int ->
  DeltaTime ->
  Input ->
  GameState ->
  Either GameError GameState
runFrames cfg n dt input = go n
 where
  go k st
    | k <= 0 = Right st
    | otherwise = do
        (_, st') <- runGameM cfg st (updateGame dt input)
        go (k - 1) st'
