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

import Domain.Logic.EnemyDamage (enemyIsAlive)
import Domain.Model.Enemy (Enemy, enemyKind)
import Domain.Model.EnemyKind (isBossKind)
import Domain.Model.ExitZone (exitZoneAabb)
import Domain.Model.GamePhase (GamePhase (..))
import Domain.Model.Player (playerAabb)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (aabbOverlaps)
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
  isLivingBoss e = isBossKind (enemyKind e) && enemyIsAlive e

hasLivingBoss :: World -> Bool
hasLivingBoss = isJust . findLivingBoss

bossDefeated :: World -> Bool
bossDefeated = not . hasLivingBoss

canCompleteLevel :: Score -> World -> Bool
canCompleteLevel s w =
  playerInExitZone w && meetsMinScore s w && bossDefeated w

resolvePlayingWin :: Int -> LevelCount -> Score -> World -> GamePhase
resolvePlayingWin levelIndex totalLevels s w
  | not (canCompleteLevel s w) = Playing
  | isFinalLevel levelIndex totalLevels = Victory
  | otherwise = LevelComplete

-- Prioridad en el mismo frame: la muerte o una vida perdida le gana a una victoria, así no podés pasar un nivel muriendo en la salida.
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

showBossExitHint :: Score -> World -> Bool
showBossExitHint s w =
  playerInExitZone w && meetsMinScore s w && hasLivingBoss w
