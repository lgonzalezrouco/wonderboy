module Domain.Logic.BossArena (
  advanceBossArenaEngagement,
  appendBossArenaWallsForPlayer,
  bossArenaSealed,
  bossArenaWallPlatforms,
  bossArenaWallsActive,
  clampPlayerInBossArena,
  playerInsideBossArena,
  playerMayDamageEnemy,
  playerWithinBossArena,
)
where

import Domain.Logic.LevelFlow (hasLivingBoss)
import Domain.Model.BossArena (BossArena (..))
import Domain.Model.Enemy (Enemy, enemyKind)
import Domain.Model.EnemyKind (isBossKind)
import Domain.Model.Platform (Platform, platform)
import Domain.Model.Player (Player (..), playerWidth)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Position (posX, position, translate)

wallThickness :: Float
wallThickness = 8.0

arenaWallHeight :: Float
arenaWallHeight = 4000.0

arenaFloorY :: Float
arenaFloorY = -2000.0

bossArenaWallPlatforms :: BossArena -> [Platform]
bossArenaWallPlatforms arena =
  [ verticalWall (bossArenaLeft arena - wallThickness)
  , verticalWall (bossArenaRight arena)
  ]
 where
  verticalWall x =
    platform (position x arenaFloorY) wallThickness arenaWallHeight

arenaFootXLimits :: BossArena -> (Float, Float)
arenaFootXLimits arena =
  ( bossArenaLeft arena + playerWidth / 2
  , bossArenaRight arena - playerWidth / 2
  )

playerInsideBossArena :: BossArena -> Player -> Bool
playerInsideBossArena arena p =
  let footX = posX (playerPos p)
      (minFootX, maxFootX) = arenaFootXLimits arena
   in footX >= minFootX && footX <= maxFootX

playerWithinBossArena :: World -> Bool
playerWithinBossArena w =
  maybe True (`playerInsideBossArena` worldPlayer w) (worldBossArena w)

bossArenaWallsActive :: World -> Bool
bossArenaWallsActive w =
  case worldBossArena w of
    Just arena
      | hasLivingBoss w ->
          worldBossArenaEngaged w
            || playerInsideBossArena arena (worldPlayer w)
    _ -> False

bossArenaSealed :: World -> Bool
bossArenaSealed w =
  worldBossArenaEngaged w && hasLivingBoss w

-- Un boss solo recibe daño mientras el jugador está dentro de su arena. Los demás enemigos siempre pueden.
playerMayDamageEnemy :: World -> Enemy -> Bool
playerMayDamageEnemy w e
  | isBossKind (enemyKind e) = playerWithinBossArena w
  | otherwise = True

-- El jugador se compromete a la pelea una vez adentro con un boss vivo. Se libera cuando el boss muere.
advanceBossArenaEngagement :: World -> World
advanceBossArenaEngagement w =
  case worldBossArena w of
    Nothing -> w{worldBossArenaEngaged = False}
    Just arena ->
      w
        { worldBossArenaEngaged =
            hasLivingBoss w
              && (worldBossArenaEngaged w || playerInsideBossArena arena (worldPlayer w))
        }

-- Respaldo de la colisión con las paredes: clampea al jugador adentro mientras la arena está sellada.
clampPlayerInBossArena :: World -> Player -> Player
clampPlayerInBossArena w p =
  case worldBossArena w of
    Just arena
      | bossArenaSealed w ->
          let footX = posX (playerPos p)
              (minFootX, maxFootX) = arenaFootXLimits arena
              clamped = max minFootX (min maxFootX footX)
           in if clamped == footX
                then p
                else p{playerPos = translate (clamped - footX) 0 (playerPos p)}
    _ -> p

appendBossArenaWallsForPlayer :: World -> [Platform] -> [Platform]
appendBossArenaWallsForPlayer w plats =
  case worldBossArena w of
    Just arena | bossArenaWallsActive w -> plats ++ bossArenaWallPlatforms arena
    _ -> plats
