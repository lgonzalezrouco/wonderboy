{- | Confinamiento del jugador en la arena de jefe (paredes invisibles).

Mientras haya un jefe vivo y el mundo tenga 'worldBossArena', se añaden
plataformas verticales efímeras a la lista de colisión del jugador.
-}
module Domain.Logic.BossArena (
  appendBossArenaWallsForPlayer,
  bossArenaWallPlatforms,
)
where

import Domain.Logic.LevelFlow (hasLivingBoss)
import Domain.Model.BossArena (BossArena (..))
import Domain.Model.Platform (Platform, platform)
import Domain.Model.World (World (..))
import Domain.ValueObjects.Position (position)

-- | Grosor de pared invisible (px).
wallThickness :: Float
wallThickness = 8.0

-- | Altura generosa para cubrir todo el plano jugable vertical.
arenaWallHeight :: Float
arenaWallHeight = 4000.0

-- | Ancla Y baja para la pared (crece hacia arriba).
arenaFloorY :: Float
arenaFloorY = -2000.0

-- | Paredes verticales en los bordes interiores @left@ / @right@.
bossArenaWallPlatforms :: BossArena -> [Platform]
bossArenaWallPlatforms arena =
  [ verticalWall (bossArenaLeft arena - wallThickness)
  , verticalWall (bossArenaRight arena)
  ]
 where
  verticalWall x =
    platform (position x arenaFloorY) wallThickness arenaWallHeight

-- | Añade paredes de arena a la lista de colisión del jugador si aplica.
appendBossArenaWallsForPlayer :: World -> [Platform] -> [Platform]
appendBossArenaWallsForPlayer w plats =
  case worldBossArena w of
    Just arena | hasLivingBoss w -> plats ++ bossArenaWallPlatforms arena
    _ -> plats
