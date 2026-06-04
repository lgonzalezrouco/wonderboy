-- | Resolución AABB jugador–plataforma (eje Y, luego eje X).
module Domain.Logic.Collision (
  resolvePlayerPlatforms,
  playerOverlapsAnyPlatform,
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
  let p' = foldl (resolveAgainst vyBefore) (p{playerOnGround = False}) plats
   in -- Cortar en cuanto la pasada alcanza un punto fijo (@p' == p@): con
      -- 'aabbOverlaps' inclusivo, un jugador apoyado exacto sobre el borde
      -- sigue "solapando", así que sin esta guarda recursaríamos hasta
      -- @maxResolvePasses@ cada frame en reposo sin mover nada.
      if p' == p || n <= 1 || not (playerOverlapsAnyPlatform plats p')
        then p'
        else resolvePasses (n - 1) vyBefore plats p'

-- | 'True' si el jugador solapa alguna plataforma de la lista.
playerOverlapsAnyPlatform :: [Platform] -> Player -> Bool
playerOverlapsAnyPlatform plats p =
  any (aabbOverlaps (playerAabb p) . platformAabb) plats

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
resolveAxisY vyBefore p box solid =
  let pushUp = aabbMaxY solid - aabbMinY box
      pushDown = aabbMaxY box - aabbMinY solid
   in if vyBefore <= 0 && pushUp > landEpsilon && pushUp <= pushDown + landEpsilon
        then landOnTop pushUp p
        else
          if vyBefore <= 0 && abs pushUp <= landEpsilon && restingOnTop box solid
            then landOnTop 0 p
            else
              if vyBefore > 0 && pushDown > landEpsilon && pushDown < pushUp
                then
                  p
                    { playerPos =
                        position (posX (playerPos p)) (posY (playerPos p) - pushDown)
                    , playerVel = velocity (velX (playerVel p)) 0
                    }
                else p

landOnTop :: Float -> Player -> Player
landOnTop pushUp p =
  p
    { playerPos = position (posX (playerPos p)) (posY (playerPos p) + pushUp)
    , playerVel = velocity (velX (playerVel p)) 0
    , playerOnGround = True
    }

restingOnTop :: Aabb -> Aabb -> Bool
restingOnTop box solid =
  abs (aabbMinY box - aabbMaxY solid) <= landEpsilon

resolveAxisX :: Player -> Aabb -> Aabb -> Player
resolveAxisX p box solid =
  let pushLeft = aabbMaxX box - aabbMinX solid
      pushRight = aabbMaxX solid - aabbMinX box
   in if pushLeft > landEpsilon && pushRight > landEpsilon
        then
          if abs (pushLeft - pushRight) <= landEpsilon
            then resolveAxisXTie pushLeft pushRight p
            else
              if pushLeft < pushRight
                then nudgeX (-pushLeft) p
                else nudgeX pushRight p
        else p

-- | Desempate simétrico: preferir la dirección del movimiento; si @vx == 0@, empujar a la izquierda.
resolveAxisXTie :: Float -> Float -> Player -> Player
resolveAxisXTie pushLeft pushRight p =
  case compare (velX (playerVel p)) 0 of
    LT -> nudgeX (-pushLeft) p
    GT -> nudgeX pushRight p
    EQ -> nudgeX (-pushLeft) p

nudgeX :: Float -> Player -> Player
nudgeX dx p =
  p{playerPos = position (posX (playerPos p) + dx) (posY (playerPos p))}
