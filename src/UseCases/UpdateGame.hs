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

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, get, modify)

import Domain.Logic.BossPhase (resolveBossPhases)
import Domain.Logic.Combat (resolveCombat)
import Domain.Logic.LevelFlow (resolveFramePhase, resolvePlayingWin)
import Domain.Logic.Pickups (resolvePickups)
import Domain.Logic.PlayerLife (resolveHazardsAndDeath)
import Domain.Logic.Projectiles (resolveProjectiles)
import Domain.Logic.Step (advanceFrame)
import Domain.Model.GamePhase (GamePhase (Playing), isSimulationFrozen)
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
'Playing' con tiempo: behaviour + física, combate, pickups, victoria híbrida,
luego out-of-bounds y muerte (la muerte anula la victoria en el mismo frame).

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
    let params = physicsParamsFromConfig cfg
        life = lifeParamsFromConfig cfg
        combat = combatParamsFromConfig cfg
        throwP = throwParamsFromConfig cfg
        livesBefore = gsLives st
        scoreAfterPickups = gsScore st
        wBefore = gsWorld st
        wAfterFrame = advanceFrame params dt input wBefore
        wAfterCombat = resolveCombat combat input wAfterFrame
        wAfterProjectiles = resolveProjectiles throwP params dt input wAfterCombat
        wAfterBoss = resolveBossPhases combat wBefore wAfterProjectiles
        (wAfterPickups, scoreDelta) = resolvePickups wAfterBoss
        scoreFinal = scoreAfterPickups <> scoreDelta
        phaseFromWin =
          resolvePlayingWin (gsLevelIndex st) (gcLevelCount cfg) scoreFinal wAfterPickups
        (wFinal, lives', phaseFromDeath) =
          resolveHazardsAndDeath life livesBefore Playing wAfterPickups
        phase' = resolveFramePhase livesBefore lives' phaseFromDeath phaseFromWin
    modify
      ( \s ->
          s
            { gsWorld = wFinal
            , gsLives = lives'
            , gsPhase = phase'
            , gsScore = scoreFinal
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
