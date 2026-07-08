{- | Confinamiento del jugador en la arena de jefe (paredes invisibles).

Mientras haya un jefe vivo y el jugador se haya comprometido con la arena
('worldBossArenaEngaged'), las paredes verticales permanecen activas aunque un
salto deje los pies fuera de los bordes interiores. La entrada desde fuera sigue
libre hasta el primer cruce.
-}
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

-- | Altura generosa para cubrir todo el plano jugable vertical.
arenaWallHeight :: Float
arenaWallHeight = 4000.0

-- | Ancla Y baja para la pared (crece hacia arriba).
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

-- | Sin arena definida no hay restricción; con arena, exige pies dentro.
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

-- | Daño del jugador a un enemigo: el jefe solo recibe golpes dentro de la arena.
playerMayDamageEnemy :: World -> Enemy -> Bool
playerMayDamageEnemy w e
  | isBossKind (enemyKind e) = playerWithinBossArena w
  | otherwise = True

-- | Marca compromiso al entrar; limpia al derrotar al jefe.
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

-- | Recorta la posición horizontal si el compromiso sigue activo (red de seguridad).
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
