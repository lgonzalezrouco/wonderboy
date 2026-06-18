-- | Resolución AABB jugador–plataforma (eje Y, luego eje X).
module Domain.Logic.Collision (
  resolvePlayerPlatforms,
  resolveEnemyPlatforms,
  playerOverlapsAnyPlatform,
  enemyOverlapsAnyPlatform,
  playerRestingOnPlatformTop,
  playerRidingPlatformTop,
)
where

import Data.List (sortBy)
import Data.Ord (comparing)

import Domain.Model.Enemy (Enemy (..), enemyAabb, enemyPos)
import Domain.Model.EnemyKind (isFlyingKind)
import Domain.Model.Platform (Platform, platformAabb)
import Domain.Model.Player (Player (..), playerAabb, playerPos)
import Domain.ValueObjects.Aabb (
  Aabb,
  aabbMaxX,
  aabbMaxY,
  aabbMinX,
  aabbMinY,
  aabbOverlaps,
 )
import Domain.ValueObjects.Position (posX, translate)
import Domain.ValueObjects.Tolerance (epsilon, nearZero)
import Domain.ValueObjects.Velocity (velX, velY, velocity)

maxResolvePasses :: Int
maxResolvePasses = 8

{- | Resuelve colisiones contra todas las plataformas; actualiza @playerOnGround@.

@vyBefore@ es la componente vertical tras input y gravedad, antes de integrar
posición; se usa para la regla 7a (solo aterrizar si @vyBefore <= 0@).

Plataformas se ordenan por borde superior descendente (suelos más altos primero)
y se repite hasta no quedar solapamiento o agotar pasadas (orden estable).
-}
resolvePlayerPlatforms :: [Platform] -> Float -> Player -> Player
resolvePlayerPlatforms plats vyBefore =
  resolvePasses maxResolvePasses vyBefore (sortPlatforms plats)

{- | Resuelve colisión enemigo–plataforma para clases terrestres.

Los enemigos voladores ('isFlyingKind') no colisionan: mantienen su ruta aérea.
Usa las mismas reglas AABB que el jugador, sin @playerOnGround@.
-}
resolveEnemyPlatforms :: [Platform] -> Enemy -> Enemy
resolveEnemyPlatforms plats e
  | isFlyingKind (enemyKind e) = e
  | otherwise =
      resolveEnemyPasses maxResolvePasses (velY (enemyVel e)) (sortPlatforms plats) e

resolveEnemyPasses :: Int -> Float -> [Platform] -> Enemy -> Enemy
resolveEnemyPasses 0 _ _ e = e
resolveEnemyPasses n vyBefore plats e =
  let e' = resolveEnemyOnce vyBefore plats e
   in if doneResolvingEnemy e e' n plats then e' else resolveEnemyPasses (n - 1) vyBefore plats e'

doneResolvingEnemy :: Enemy -> Enemy -> Int -> [Platform] -> Bool
doneResolvingEnemy e e' n plats =
  e' == e || n <= 1 || not (enemyOverlapsAnyPlatform plats e)

resolveEnemyOnce :: Float -> [Platform] -> Enemy -> Enemy
resolveEnemyOnce vyBefore plats e =
  foldl (resolveEnemyAgainst vyBefore) e plats

enemyOverlapsAnyPlatform :: [Platform] -> Enemy -> Bool
enemyOverlapsAnyPlatform plats e =
  let box = enemyAabb e
   in any (aabbOverlaps box . platformAabb) plats

resolveEnemyAgainst :: Float -> Enemy -> Platform -> Enemy
resolveEnemyAgainst vyBefore e plat =
  let box = enemyAabb e
      solid = platformAabb plat
   in if aabbOverlaps box solid
        then resolveEnemyOverlap vyBefore e box solid
        else e

resolveEnemyOverlap :: Float -> Enemy -> Aabb -> Aabb -> Enemy
resolveEnemyOverlap vyBefore e box solid =
  let eY = resolveEnemyAxisY vyBefore e box solid
      box' = enemyAabb eY
   in if eY /= e || restingOnTop box' solid || touchingCeiling box' solid
        then eY
        else resolveEnemyAxisX eY box' solid

resolveEnemyAxisY :: Float -> Enemy -> Aabb -> Aabb -> Enemy
resolveEnemyAxisY vyBefore e box solid
  | vyBefore <= 0
  , pushUp > epsilon
  , pushUp <= pushDown + epsilon =
      landEnemyOnTop pushUp e
  | vyBefore <= 0
  , nearZero pushUp
  , restingOnTop box solid =
      landEnemyOnTop 0 e
  | vyBefore > 0
  , pushDown > epsilon
  , pushDown < pushUp =
      bumpEnemyCeiling pushDown e
  | otherwise =
      e
 where
  (pushUp, pushDown) = separationsY box solid

landEnemyOnTop :: Float -> Enemy -> Enemy
landEnemyOnTop pushUp = zeroEnemyVy . nudgeEnemyY pushUp

bumpEnemyCeiling :: Float -> Enemy -> Enemy
bumpEnemyCeiling pushDown = zeroEnemyVy . nudgeEnemyY (-pushDown)

resolveEnemyAxisX :: Enemy -> Aabb -> Aabb -> Enemy
resolveEnemyAxisX e box solid =
  case horizontalNudge (separationsX box solid) (velX (enemyVel e)) of
    Nothing -> e
    Just dx -> zeroEnemyVx . nudgeEnemyX dx $ e

zeroEnemyVy :: Enemy -> Enemy
zeroEnemyVy e = e{enemyVel = velocity (velX (enemyVel e)) 0}

zeroEnemyVx :: Enemy -> Enemy
zeroEnemyVx e = e{enemyVel = velocity 0 (velY (enemyVel e))}

nudgeEnemyX :: Float -> Enemy -> Enemy
nudgeEnemyX dx e =
  e{enemyPos = translate dx 0 (enemyPos e)}

nudgeEnemyY :: Float -> Enemy -> Enemy
nudgeEnemyY dy e =
  e{enemyPos = translate 0 dy (enemyPos e)}

sortPlatforms :: [Platform] -> [Platform]
sortPlatforms =
  sortBy (comparing (negate . aabbMaxY . platformAabb))

resolvePasses :: Int -> Float -> [Platform] -> Player -> Player
resolvePasses 0 _ _ p = p
resolvePasses n vyBefore plats p =
  let p' = resolveOnce vyBefore plats p
   in -- Cortar en cuanto la pasada alcanza un punto fijo (@p' == p@): con
      -- 'aabbOverlaps' inclusivo, un jugador apoyado exacto sobre el borde
      -- sigue "solapando", así que sin esta guarda recursaríamos hasta
      -- @maxResolvePasses@ cada frame en reposo sin mover nada.
      if doneResolving p p' n plats then p' else resolvePasses (n - 1) vyBefore plats p'

doneResolving :: Player -> Player -> Int -> [Platform] -> Bool
doneResolving p p' n plats =
  p' == p || n <= 1 || not (playerOverlapsAnyPlatform plats p')

resolveOnce :: Float -> [Platform] -> Player -> Player
resolveOnce vyBefore plats p =
  foldl (resolveAgainst vyBefore) (p{playerOnGround = False}) plats

-- | 'True' si el jugador solapa alguna plataforma de la lista.
playerOverlapsAnyPlatform :: [Platform] -> Player -> Bool
playerOverlapsAnyPlatform plats p =
  let box = playerAabb p
   in any (aabbOverlaps box . platformAabb) plats

resolveAgainst :: Float -> Player -> Platform -> Player
resolveAgainst _vyBefore p plat =
  let box = playerAabb p
      solid = platformAabb plat
   in if aabbOverlaps box solid
        then resolveOverlap (velY (playerVel p)) p box solid
        else p

resolveOverlap :: Float -> Player -> Aabb -> Aabb -> Player
resolveOverlap vyBefore p box solid =
  let pY = resolveAxisY vyBefore p box solid
      box' = playerAabb pY
   in if pY /= p || restingOnTop box' solid || touchingCeiling box' solid
        then pY
        else resolveAxisX pY box' solid

resolveAxisY :: Float -> Player -> Aabb -> Aabb -> Player
resolveAxisY vyBefore p box solid
  | vyBefore <= 0
  , pushUp > epsilon
  , pushUp <= pushDown + epsilon =
      landOnTop pushUp p
  | vyBefore <= 0
  , nearZero pushUp
  , restingOnTop box solid =
      landOnTop 0 p
  | vyBefore > 0
  , pushDown > epsilon
  , pushDown < pushUp =
      bumpCeiling pushDown p
  | otherwise =
      p
 where
  (pushUp, pushDown) = separationsY box solid

landOnTop :: Float -> Player -> Player
landOnTop pushUp p =
  (zeroVy . nudgeY pushUp $ p){playerOnGround = True}

bumpCeiling :: Float -> Player -> Player
bumpCeiling pushDown = zeroVy . nudgeY (-pushDown)

restingOnTop :: Aabb -> Aabb -> Bool
restingOnTop box solid =
  nearZero (aabbMinY box - aabbMaxY solid)

touchingCeiling :: Aabb -> Aabb -> Bool
touchingCeiling box solid =
  nearZero (aabbMaxY box - aabbMinY solid)

-- | 'True' si los pies del jugador apoyan el borde superior de la plataforma.
playerRestingOnPlatformTop :: Player -> Platform -> Bool
playerRestingOnPlatformTop p plat =
  restingOnTop (playerAabb p) (platformAabb plat)

-- | 'True' si el jugador está montado sobre la plataforma (pies sobre el tramo superior).
playerRidingPlatformTop :: Player -> Platform -> Bool
playerRidingPlatformTop p plat =
  let solid = platformAabb plat
      footX = posX (playerPos p)
   in playerRestingOnPlatformTop p plat
        && footX >= aabbMinX solid
        && footX <= aabbMaxX solid

resolveAxisX :: Player -> Aabb -> Aabb -> Player
resolveAxisX p box solid =
  case horizontalNudge (separationsX box solid) (velX (playerVel p)) of
    Nothing -> p
    Just dx -> nudgeX dx p

horizontalNudge :: (Float, Float) -> Float -> Maybe Float
horizontalNudge (pushLeft, pushRight) vx
  | pushLeft <= epsilon || pushRight <= epsilon = Nothing
  | vx > 0 = Just (-pushLeft)
  | vx < 0 = Just pushRight
  | pushLeft < pushRight - epsilon = Just (-pushLeft)
  | pushRight < pushLeft - epsilon = Just pushRight
  | otherwise = Just (-pushLeft)

separationsY :: Aabb -> Aabb -> (Float, Float)
separationsY box solid =
  (aabbMaxY solid - aabbMinY box, aabbMaxY box - aabbMinY solid)

separationsX :: Aabb -> Aabb -> (Float, Float)
separationsX box solid =
  (aabbMaxX box - aabbMinX solid, aabbMaxX solid - aabbMinX box)

zeroVy :: Player -> Player
zeroVy p = p{playerVel = velocity (velX (playerVel p)) 0}

nudgeX :: Float -> Player -> Player
nudgeX dx p =
  p{playerPos = translate dx 0 (playerPos p)}

nudgeY :: Float -> Player -> Player
nudgeY dy p =
  p{playerPos = translate 0 dy (playerPos p)}
