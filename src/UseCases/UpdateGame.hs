{- | Orquestación del ciclo de actualización del juego.

'updateGame' lee 'GameConfig' con 'MonadReader', aplica la política de frame
congelado y fases terminales, y eleva 'Domain.Logic.Frame.advanceSimulationFrame'
al estado del juego con 'MonadState'.
'runFrames' corre @n@ frames seguidos reutilizando 'runGameM'.
-}
module UseCases.UpdateGame (
  -- * Ciclo de update
  updateGame,

  -- * Simulación de varios frames
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

{- | Actualiza el estado del juego para un frame dado.

Con 'GameOver', 'LevelComplete' o 'Victory' no avanza simulación. Con el frame
congelado ('Domain.ValueObjects.DeltaTime.isFrozen') tampoco avanza nada. En
'Playing' con tiempo delega en 'advanceSimulationFrame'.

La firma es polimórfica en las typeclasses 'mtl' que realmente usa
('MonadReader' 'GameConfig', 'MonadState' 'GameState'); no necesita 'MonadError'
porque la transición de frame es total. 'GameM' es una instancia válida.
-}
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

{- | Corre @n@ frames consecutivos con el mismo @dt@ e 'Input', o el primer error.

Con @n <= 0@ devuelve el estado sin tocarlo.
-}
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
