-- | Resolución AABB jugador–plataforma (eje Y, luego eje X).
module Domain.Logic.Collision (
  resolvePlayerPlatforms,
  playerOverlapsAnyPlatform,
  playerRestingOnPlatformTop,
  landEpsilon,
)
where

import Data.List (sortBy)
import Data.Ord (comparing)

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
import Domain.ValueObjects.Position (posX, posY, position)
import Domain.ValueObjects.Velocity (velX, velocity)

landEpsilon :: Float
landEpsilon = 1e-3

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
resolveAgainst vyBefore p plat =
  let box = playerAabb p
      solid = platformAabb plat
   in if aabbOverlaps box solid
        then resolveOverlap vyBefore p box solid
        else p

resolveOverlap :: Float -> Player -> Aabb -> Aabb -> Player
resolveOverlap vyBefore p box solid =
  let pY = resolveAxisY vyBefore p box solid
      box' = playerAabb pY
   in if restingOnTop box' solid
        then pY
        else resolveAxisX pY box' solid

resolveAxisY :: Float -> Player -> Aabb -> Aabb -> Player
resolveAxisY vyBefore p box solid
  | vyBefore <= 0
  , pushUp > landEpsilon
  , pushUp <= pushDown + landEpsilon =
      landOnTop pushUp p
  | vyBefore <= 0
  , nearZero pushUp
  , restingOnTop box solid =
      landOnTop 0 p
  | vyBefore > 0
  , pushDown > landEpsilon
  , pushDown < pushUp =
      bumpCeiling pushDown p
  | otherwise =
      p
 where
  (pushUp, pushDown) = separationsY box solid

nearZero :: Float -> Bool
nearZero x = abs x <= landEpsilon

landOnTop :: Float -> Player -> Player
landOnTop pushUp p =
  (zeroVy . nudgeY pushUp $ p){playerOnGround = True}

bumpCeiling :: Float -> Player -> Player
bumpCeiling pushDown = zeroVy . nudgeY (-pushDown)

restingOnTop :: Aabb -> Aabb -> Bool
restingOnTop box solid =
  nearZero (aabbMinY box - aabbMaxY solid)

-- | 'True' si los pies del jugador apoyan el borde superior de la plataforma.
playerRestingOnPlatformTop :: Player -> Platform -> Bool
playerRestingOnPlatformTop p plat =
  restingOnTop (playerAabb p) (platformAabb plat)

resolveAxisX :: Player -> Aabb -> Aabb -> Player
resolveAxisX p box solid =
  case horizontalNudge (separationsX box solid) (velX (playerVel p)) of
    Nothing -> p
    Just dx -> nudgeX dx p

horizontalNudge :: (Float, Float) -> Float -> Maybe Float
horizontalNudge (pushLeft, pushRight) vx
  | pushLeft <= landEpsilon || pushRight <= landEpsilon = Nothing
  | nearZero (pushLeft - pushRight) =
      Just $
        case compare vx 0 of
          LT -> -pushLeft
          GT -> pushRight
          EQ -> -pushLeft
  | pushLeft < pushRight = Just (-pushLeft)
  | otherwise = Just pushRight

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
  p{playerPos = position (posX (playerPos p) + dx) (posY (playerPos p))}

nudgeY :: Float -> Player -> Player
nudgeY dy p =
  p{playerPos = position (posX (playerPos p)) (posY (playerPos p) + dy)}
