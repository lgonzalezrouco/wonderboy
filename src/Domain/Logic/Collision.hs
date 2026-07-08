module Domain.Logic.Collision (
  resolvePlayerPlatforms,
  resolveEnemyPlatforms,
  playerOverlapsAnyPlatform,
  enemyOverlapsAnyPlatform,
  playerRestingOnPlatformTop,
  playerRidingPlatformTop,
)
where

import Data.List (foldl', sortBy)
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
import Domain.ValueObjects.Position (Position, posX, translate)
import Domain.ValueObjects.Tolerance (epsilon, nearZero)
import Domain.ValueObjects.Velocity (Velocity, velX, velY, velocity)

maxResolvePasses :: Int
maxResolvePasses = 8

-- Accesores/mutadores compartidos para resolver solapes cuerpo–plataforma.
data BodyOps a = BodyOps
  { bodyAabb :: a -> Aabb
  , bodyVel :: a -> Velocity
  , bodyPos :: a -> Position
  , setBodyVel :: Velocity -> a -> a
  , setBodyPos :: Position -> a -> a
  , onLand :: a -> a
  , beforePass :: a -> a
  , zeroVxOnWall :: Bool
  , overlapVy :: Float -> a -> Float
  , doneOverlapBody :: a -> a -> a
  }

playerOps :: BodyOps Player
playerOps =
  BodyOps
    { bodyAabb = playerAabb
    , bodyVel = playerVel
    , bodyPos = playerPos
    , setBodyVel = \v p -> p{playerVel = v}
    , setBodyPos = \pos p -> p{playerPos = pos}
    , onLand = \p -> p{playerOnGround = True}
    , beforePass = \p -> p{playerOnGround = False}
    , zeroVxOnWall = False
    , overlapVy = \_ p -> velY (playerVel p)
    , doneOverlapBody = \_ after -> after
    }

enemyOps :: BodyOps Enemy
enemyOps =
  BodyOps
    { bodyAabb = enemyAabb
    , bodyVel = enemyVel
    , bodyPos = enemyPos
    , setBodyVel = \v e -> e{enemyVel = v}
    , setBodyPos = \pos e -> e{enemyPos = pos}
    , onLand = id
    , beforePass = id
    , zeroVxOnWall = True
    , overlapVy = const
    , doneOverlapBody = const
    }

resolvePlayerPlatforms :: [Platform] -> Float -> Player -> Player
resolvePlayerPlatforms plats vyBefore =
  resolvePasses playerOps maxResolvePasses vyBefore (sortPlatforms plats)

resolveEnemyPlatforms :: [Platform] -> Float -> Enemy -> Enemy
resolveEnemyPlatforms plats vyBefore e
  | isFlyingKind (enemyKind e) = e
  | otherwise =
      resolvePasses enemyOps maxResolvePasses vyBefore (sortPlatforms plats) e

enemyOverlapsAnyPlatform :: [Platform] -> Enemy -> Bool
enemyOverlapsAnyPlatform = overlapsAnyPlatform enemyOps

playerOverlapsAnyPlatform :: [Platform] -> Player -> Bool
playerOverlapsAnyPlatform = overlapsAnyPlatform playerOps

playerRestingOnPlatformTop :: Player -> Platform -> Bool
playerRestingOnPlatformTop p plat =
  restingOnTop (playerAabb p) (platformAabb plat)

playerRidingPlatformTop :: Player -> Platform -> Bool
playerRidingPlatformTop p plat =
  let solid = platformAabb plat
      footX = posX (playerPos p)
   in playerRestingOnPlatformTop p plat
        && footX >= aabbMinX solid
        && footX <= aabbMaxX solid

sortPlatforms :: [Platform] -> [Platform]
sortPlatforms =
  sortBy (comparing (negate . aabbMaxY . platformAabb))

resolvePasses :: (Eq a) => BodyOps a -> Int -> Float -> [Platform] -> a -> a
resolvePasses _ 0 _ _ body = body
resolvePasses ops n vyBefore plats body =
  let body' = resolveOnce ops vyBefore plats body
   in if doneResolving ops body body' n plats
        then body'
        else resolvePasses ops (n - 1) vyBefore plats body'

-- Se detiene en un punto fijo (body' == body): aabbOverlaps es inclusivo, así que un cuerpo
-- apoyado justo sobre un borde igual "solapa". Sin esto quemaríamos todas las pasadas en cada frame quieto.
doneResolving :: (Eq a) => BodyOps a -> a -> a -> Int -> [Platform] -> Bool
doneResolving ops body body' n plats =
  body' == body
    || n <= 1
    || not (overlapsAnyPlatform ops plats (doneOverlapBody ops body body'))

resolveOnce :: (Eq a) => BodyOps a -> Float -> [Platform] -> a -> a
resolveOnce ops vyBefore plats body =
  foldl' (resolveAgainst ops vyBefore) (beforePass ops body) plats

overlapsAnyPlatform :: BodyOps a -> [Platform] -> a -> Bool
overlapsAnyPlatform ops plats body =
  let box = bodyAabb ops body
   in any (aabbOverlaps box . platformAabb) plats

resolveAgainst :: (Eq a) => BodyOps a -> Float -> a -> Platform -> a
resolveAgainst ops vyBefore body plat =
  let box = bodyAabb ops body
      solid = platformAabb plat
   in if aabbOverlaps box solid
        then resolveOverlap ops (overlapVy ops vyBefore body) body box solid
        else body

-- Empuja por el eje de menor penetración: el solape más chico es el que el cuerpo
-- acaba de cruzar, así que corregir ese es lo que de verdad los separa.
resolveOverlap :: (Eq a) => BodyOps a -> Float -> a -> Aabb -> Aabb -> a
resolveOverlap ops vyBefore body box solid
  | overlapX + epsilon < overlapY = resolveAxisX ops body box solid
  | otherwise =
      let bodyY = resolveAxisY ops vyBefore body box solid
          box' = bodyAabb ops bodyY
       in if bodyY /= body || restingOnTop box' solid || touchingCeiling box' solid
            then bodyY
            else resolveAxisX ops bodyY box' solid
 where
  overlapX = uncurry min (separationsX box solid)
  overlapY = uncurry min (separationsY box solid)

resolveAxisY :: BodyOps a -> Float -> a -> Aabb -> Aabb -> a
resolveAxisY ops vyBefore body box solid
  | vyBefore <= 0
  , pushUp > epsilon
  , pushUp <= pushDown + epsilon =
      landOnTop ops pushUp body
  | vyBefore <= 0
  , nearZero pushUp
  , restingOnTop box solid =
      landOnTop ops 0 body
  | vyBefore > 0
  , pushDown > epsilon
  , pushDown < pushUp =
      bumpCeiling ops pushDown body
  | otherwise =
      body
 where
  (pushUp, pushDown) = separationsY box solid

landOnTop :: BodyOps a -> Float -> a -> a
landOnTop ops pushUp body =
  onLand ops (zeroVy ops (nudgeY ops pushUp body))

bumpCeiling :: BodyOps a -> Float -> a -> a
bumpCeiling ops pushDown = zeroVy ops . nudgeY ops (-pushDown)

restingOnTop :: Aabb -> Aabb -> Bool
restingOnTop box solid =
  nearZero (aabbMinY box - aabbMaxY solid)

touchingCeiling :: Aabb -> Aabb -> Bool
touchingCeiling box solid =
  nearZero (aabbMaxY box - aabbMinY solid)

resolveAxisX :: BodyOps a -> a -> Aabb -> Aabb -> a
resolveAxisX ops body box solid =
  case horizontalNudge (separationsX box solid) (velX (bodyVel ops body)) of
    Nothing -> body
    Just dx ->
      let nudged = nudgeX ops dx body
       in if zeroVxOnWall ops then zeroVx ops nudged else nudged

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

zeroVy :: BodyOps a -> a -> a
zeroVy ops body =
  let v = bodyVel ops body
   in setBodyVel ops (velocity (velX v) 0) body

zeroVx :: BodyOps a -> a -> a
zeroVx ops body =
  let v = bodyVel ops body
   in setBodyVel ops (velocity 0 (velY v)) body

nudgeX :: BodyOps a -> Float -> a -> a
nudgeX ops dx body =
  setBodyPos ops (translate dx 0 (bodyPos ops body)) body

nudgeY :: BodyOps a -> Float -> a -> a
nudgeY ops dy body =
  setBodyPos ops (translate 0 dy (bodyPos ops body)) body
