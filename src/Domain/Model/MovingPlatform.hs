{- | Plataforma móvil del nivel: sólido con trayectoria ping-pong entre dos extremos.

Las plataformas móviles comparten el ancla bottom-left con 'Platform' estática.
Los extremos @EndA@ y @EndB@ son posiciones del ancla en cada extremo del recorrido
(eje horizontal o vertical, no diagonal).
-}
module Domain.Model.MovingPlatform (
  -- * Tipo
  MovingPlatform (..),

  -- * Construcción
  mkMovingPlatform,

  -- * Geometría
  movingPlatformAabb,
  movingPlatformAsPlatform,
  movingPlatformIsHorizontal,
)
where

import Domain.Model.Platform (Platform, platform)
import Domain.ValueObjects.Aabb (Aabb, aabbFromBottomLeft)
import Domain.ValueObjects.Position (Position, posX, posY)
import Domain.ValueObjects.Tolerance (epsilon, near)

{- | Plataforma con movimiento ping-pong entre @movingPlatformEndA@ y @EndB@.

@movingPlatformPos@ es la esquina inferior izquierda en el frame actual.
@movingPlatformTowardB@ indica si el ancla viaja hacia @EndB@ este frame.
-}
data MovingPlatform = MovingPlatform
  { movingPlatformId :: Int
  , movingPlatformPos :: Position
  , movingPlatformWidth :: Float
  , movingPlatformHeight :: Float
  , movingPlatformEndA :: Position
  , movingPlatformEndB :: Position
  , movingPlatformSpeed :: Float
  , movingPlatformTowardB :: Bool
  }
  deriving (Eq, Show)

-- | Crea una plataforma móvil validando geometría y extremos alineados a un eje.
mkMovingPlatform ::
  Int ->
  Position ->
  Float ->
  Float ->
  Position ->
  Position ->
  Float ->
  Bool ->
  Maybe MovingPlatform
mkMovingPlatform pid pos width height endA endB speed towardB
  | width <= 0 || height <= 0 || speed <= 0 = Nothing
  | not (onSegment pos endA endB) = Nothing
  | isHorizontal endA endB || isVertical endA endB =
      Just
        MovingPlatform
          { movingPlatformId = pid
          , movingPlatformPos = pos
          , movingPlatformWidth = width
          , movingPlatformHeight = height
          , movingPlatformEndA = endA
          , movingPlatformEndB = endB
          , movingPlatformSpeed = speed
          , movingPlatformTowardB = towardB
          }
  | otherwise = Nothing

isHorizontal :: Position -> Position -> Bool
isHorizontal a b =
  near (posY a) (posY b) && not (near (posX a) (posX b))

isVertical :: Position -> Position -> Bool
isVertical a b =
  near (posX a) (posX b) && not (near (posY a) (posY b))

onSegment :: Position -> Position -> Position -> Bool
onSegment pos endA endB
  | isHorizontal endA endB =
      near (posY pos) (posY endA)
        && inRange (posX pos) (posX endA) (posX endB)
  | isVertical endA endB =
      near (posX pos) (posX endA)
        && inRange (posY pos) (posY endA) (posY endB)
  | otherwise = False

inRange :: Float -> Float -> Float -> Bool
inRange v a b =
  let lo = min a b
      hi = max a b
   in v >= lo - epsilon && v <= hi + epsilon

{- | 'True' si la plataforma recorre el eje horizontal.

Única clasificación de eje del tipo: la calcula sobre los extremos ya validados
(alineados a un eje, no degenerados) por 'mkMovingPlatform', de modo que el avance
por frame en @Domain.Logic.MovingPlatforms@ no re-deriva la pregunta con otra regla.
-}
movingPlatformIsHorizontal :: MovingPlatform -> Bool
movingPlatformIsHorizontal mp =
  isHorizontal (movingPlatformEndA mp) (movingPlatformEndB mp)

-- | Caja de colisión (bottom-left anchor).
movingPlatformAabb :: MovingPlatform -> Aabb
movingPlatformAabb mp =
  aabbFromBottomLeft
    (movingPlatformPos mp)
    (movingPlatformWidth mp)
    (movingPlatformHeight mp)

-- | Instantánea estática para reutilizar resolución AABB del jugador.
movingPlatformAsPlatform :: MovingPlatform -> Platform
movingPlatformAsPlatform mp =
  platform
    (movingPlatformPos mp)
    (movingPlatformWidth mp)
    (movingPlatformHeight mp)
