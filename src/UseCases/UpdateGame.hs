{- | Orquestación del ciclo de actualización del juego.

'updateGame' es el punto de entrada para un frame de simulación: lee la
configuración con 'MonadReader', y eleva la transición pura de frame
('Domain.Logic.Step.advanceFrame' + 'Domain.Logic.PlayerLife.resolveHazardsAndDeath')
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

import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify)

import Domain.Logic.PlayerLife (resolveHazardsAndDeath)
import Domain.Logic.Step (advanceFrame)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.ValueObjects.DeltaTime (DeltaTime)
import Domain.ValueObjects.Input (Input)
import UseCases.GameMonad (
  GameConfig,
  GameError,
  GameM,
  GameState (..),
  lifeParamsFromConfig,
  physicsParamsFromConfig,
  runGameM,
 )

{- | Actualiza el estado del juego para un frame dado.

Con 'GameOver' no avanza simulación ni aplica input. En 'Playing': behaviour +
física, luego out-of-bounds y resolución de muerte.
-}
updateGame :: DeltaTime -> Input -> GameM ()
updateGame dt input = do
  st <- get
  case gsPhase st of
    GameOver -> pure ()
    Playing -> do
      cfg <- ask
      let params = physicsParamsFromConfig cfg
          life = lifeParamsFromConfig cfg
          w' = advanceFrame params dt input (gsWorld st)
          (w'', lives', phase') =
            resolveHazardsAndDeath life (gsLives st) Playing w'
      modify
        ( \s ->
            s{gsWorld = w'', gsLives = lives', gsPhase = phase'}
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
