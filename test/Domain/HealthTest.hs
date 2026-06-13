{- | Reglas puras de daño, respawn y game over (M9). -}
module Domain.HealthTest where

import Domain.Fixtures (dtFrame, testParams)
import Domain.Logic.Health (applyDamage, resolveLifeLoss)
import Domain.Logic.Step (advanceFrame)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (
  playerHealth,
  playerMaxHealth,
  playerPos,
  spawnPlayer,
 )
import Domain.Model.World (World (..), mkTestWorld)
import Domain.ValueObjects.Input (noInput)
import Domain.ValueObjects.Position (Position, posX, posY, position)
import Test.Tasty.HUnit (Assertion, (@?=))

unit_applyDamageReducesHealth :: Assertion
unit_applyDamageReducesHealth = do
  let spawn = position 10 20
      w0 = mkTestWorld spawn (spawnPlayer spawn) [] []
      w1 = applyDamage 1 w0
  playerHealth (worldPlayer w1) @?= 2
  worldLives w1 @?= worldLives w0

unit_applyDamageFloorsAtZero :: Assertion
unit_applyDamageFloorsAtZero = do
  let spawn = position 0 0
      w0 =
        (mkTestWorld spawn (spawnPlayer spawn) [] [])
          { worldPlayer = (spawnPlayer spawn){playerHealth = 1}
          }
      w1 = applyDamage 5 w0
  playerHealth (worldPlayer w1) @?= 0

unit_deathWithLivesRemainingRespawns :: Assertion
unit_deathWithLivesRemainingRespawns = do
  let spawn = position 12 34
      w0 = deadAtSpawn spawn 3
      w1 = resolveLifeLoss w0
  worldLives w1 @?= 2
  playerHealth (worldPlayer w1) @?= playerMaxHealth
  posX (playerPos (worldPlayer w1)) @?= posX spawn
  posY (playerPos (worldPlayer w1)) @?= posY spawn
  worldPhase w1 @?= Playing

unit_lastLifeGameOver :: Assertion
unit_lastLifeGameOver = do
  let spawn = position 0 0
      w0 = deadAtSpawn spawn 1
      w1 = resolveLifeLoss w0
  worldLives w1 @?= 0
  worldPhase w1 @?= GameOver

unit_gameOverSkipsPhysics :: Assertion
unit_gameOverSkipsPhysics = do
  let spawn = position 0 80
      w0 =
        (mkTestWorld spawn (spawnPlayer spawn) [] [])
          { worldPhase = GameOver
          , worldLives = 0
          }
      w1 = advanceFrame testParams dtFrame noInput w0
  w1 @?= w0

unit_resolveLifeLossIdempotent :: Assertion
unit_resolveLifeLossIdempotent = do
  let spawn = position 5 5
      w0 = deadAtSpawn spawn 3
      w1 = resolveLifeLoss w0
      w2 = resolveLifeLoss w1
  w2 @?= w1

unit_advanceFrameResolvesDeath :: Assertion
unit_advanceFrameResolvesDeath = do
  let spawn = position 0 0
      w0 = deadAtSpawn spawn 2
      w1 = advanceFrame testParams dtFrame noInput w0
  worldLives w1 @?= 1
  playerHealth (worldPlayer w1) @?= playerMaxHealth

-- | Jugador muerto en spawn con @lives@ vidas restantes.
deadAtSpawn :: Position -> Int -> World
deadAtSpawn spawn lives =
  (mkTestWorld spawn (spawnPlayer spawn) [] [])
    { worldLives = lives
    , worldPlayer = (spawnPlayer spawn){playerHealth = 0}
    }
