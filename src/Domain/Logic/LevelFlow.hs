{- | Flujo de nivel: victoria híbrida y transiciones de fase de juego.

Comprueba superposición con la zona de salida, puntuación mínima y jefe
derrotado antes de avanzar la partida.
-}
module Domain.Logic.LevelFlow (
  playerInExitZone,
  meetsMinScore,
  findLivingBoss,
  hasLivingBoss,
  bossDefeated,
  canCompleteLevel,
  resolvePlayingWin,
  resolveFramePhase,
  showExitScoreHint,
  showBossExitHint,
)
where

import Data.List (find)
import Data.Maybe (isJust)

import Domain.Model.Enemy (Enemy, enemyHealth, enemyKind)
import Domain.Model.EnemyKind (isBossKind)
import Domain.Model.ExitZone (exitZoneAabb)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (playerAabb)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (aabbOverlaps)
import Domain.ValueObjects.Health (isDepleted)
import Domain.ValueObjects.LevelCount (LevelCount, isFinalLevel)
import Domain.ValueObjects.Lives (Lives, livesCount)
import Domain.ValueObjects.Score (Score)

playerInExitZone :: World -> Bool
playerInExitZone w =
  aabbOverlaps (playerAabb (worldPlayer w)) (exitZoneAabb (worldExit w))

meetsMinScore :: Score -> World -> Bool
meetsMinScore s w = s >= worldMinScore w

findLivingBoss :: World -> Maybe Enemy
findLivingBoss w = find isLivingBoss (worldEnemies w)
 where
  isLivingBoss e = isBossKind (enemyKind e) && not (isDepleted (enemyHealth e))

hasLivingBoss :: World -> Bool
hasLivingBoss = isJust . findLivingBoss

-- | 'True' cuando no hay jefe vivo (nivel sin jefe o jefe derrotado).
bossDefeated :: World -> Bool
bossDefeated = not . hasLivingBoss

-- | Condición de victoria híbrida para el nivel actual.
canCompleteLevel :: Score -> World -> Bool
canCompleteLevel s w =
  playerInExitZone w && meetsMinScore s w && bossDefeated w

resolvePlayingWin :: Int -> LevelCount -> Score -> World -> GamePhase
resolvePlayingWin levelIndex totalLevels s w
  | not (canCompleteLevel s w) = Playing
  | isFinalLevel levelIndex totalLevels = Victory
  | otherwise = LevelComplete

-- | Prioridad de fase tras victoria híbrida y peligros en el mismo frame.
resolveFramePhase ::
  Lives ->
  Lives ->
  GamePhase ->
  GamePhase ->
  GamePhase
resolveFramePhase livesBefore livesAfter phaseFromDeath phaseFromWin =
  case phaseFromDeath of
    GameOver -> GameOver
    _ | livesCount livesAfter < livesCount livesBefore -> Playing
    _ -> phaseFromWin

showExitScoreHint :: Score -> World -> Bool
showExitScoreHint s w =
  playerInExitZone w && not (meetsMinScore s w)

-- | Hint de jefe vivo en la salida (solo cuando la puntuación ya alcanza el mínimo).
showBossExitHint :: Score -> World -> Bool
showBossExitHint s w =
  playerInExitZone w && meetsMinScore s w && hasLivingBoss w
