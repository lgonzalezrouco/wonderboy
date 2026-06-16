-- | Avance ping-pong y desplazamiento (carry) de plataformas m?viles.
module Domain.Logic.MovingPlatforms (
  MovingPlatformAdvance (..),
  advanceMovingPlatforms,
  applyPrePhysicsCarry,
  allCollisionPlatforms,
)
where

import Domain.Logic.Collision (landEpsilon, playerRidingPlatformTop)
import Domain.Model.MovingPlatform (
  MovingPlatform (..),
  movingPlatformAsPlatform,
 )
import Domain.Model.Platform (Platform)
import Domain.Model.Player (Player (..), playerOnGround, playerPos)
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Position (Position, posX, posY, position)

-- | Resultado de avanzar una plataforma m?vil un frame.
data MovingPlatformAdvance = MovingPlatformAdvance
  { mpaPlatform :: MovingPlatform
  , mpaDeltaX :: Float
  , mpaDeltaY :: Float
  }
  deriving (Eq, Show)

-- | Avanza todas las plataformas m?viles y registra el delta de posici?n por una.
advanceMovingPlatforms :: DeltaTime -> [MovingPlatform] -> [MovingPlatformAdvance]
advanceMovingPlatforms dt =
  map (advanceOne dt)

advanceOne :: DeltaTime -> MovingPlatform -> MovingPlatformAdvance
advanceOne dt mp =
  let oldPos = movingPlatformPos mp
      dist = movingPlatformSpeed mp * seconds dt
      mp' = advanceBy dist mp
      newPos = movingPlatformPos mp'
      dx = posX newPos - posX oldPos
      dy = posY newPos - posY oldPos
   in MovingPlatformAdvance mp' dx dy

advanceBy :: Float -> MovingPlatform -> MovingPlatform
advanceBy dist mp =
  let pos = movingPlatformPos mp
      target = currentTarget mp
      (newPos, reached) =
        if isHorizontal mp
          then
            let (newX, hit) = moveAlongAxis (posX pos) (posX target) dist
             in (position newX (posY pos), hit)
          else
            let (newY, hit) = moveAlongAxis (posY pos) (posY target) dist
             in (position (posX pos) newY, hit)
   in mp
        { movingPlatformPos = newPos
        , movingPlatformTowardB = flipTowardIfReached mp reached
        }

currentTarget :: MovingPlatform -> Position
currentTarget mp =
  if movingPlatformTowardB mp
    then movingPlatformEndB mp
    else movingPlatformEndA mp

flipTowardIfReached :: MovingPlatform -> Bool -> Bool
flipTowardIfReached mp reached =
  if reached then not (movingPlatformTowardB mp) else movingPlatformTowardB mp

isHorizontal :: MovingPlatform -> Bool
isHorizontal mp =
  near (posY (movingPlatformEndA mp)) (posY (movingPlatformEndB mp))

moveAlongAxis :: Float -> Float -> Float -> (Float, Bool)
moveAlongAxis cur target dist
  | abs (target - cur) <= dist + landEpsilon = (target, True)
  | otherwise =
      let dir = signum (target - cur)
       in (cur + dir * dist, False)

near :: Float -> Float -> Bool
near x y = abs (x - y) <= landEpsilon

-- | Plataformas est?ticas m?s instant?neas de las m?viles para colisi?n del jugador.
allCollisionPlatforms :: [Platform] -> [MovingPlatform] -> [Platform]
allCollisionPlatforms static moving =
  static ++ map movingPlatformAsPlatform moving

{- | Aplica el delta /antes/ de integrar f?sica: el jugador se apoya sobre la
posici?n previa de la plataforma; la colisi?n posterior usa la posici?n nueva
sin volver a sumar el desplazamiento (evita doble carry en eje Y).
-}
applyPrePhysicsCarry :: Player -> [MovingPlatformAdvance] -> Player
applyPrePhysicsCarry =
  foldl applyOne
 where
  applyOne player adv =
    let oldMp = movingPlatformBeforeAdvance adv
     in if playerOnGround player
          && playerRidingPlatformTop player (movingPlatformAsPlatform oldMp)
          then nudgePlayer (mpaDeltaX adv) (mpaDeltaY adv) player
          else player

movingPlatformBeforeAdvance :: MovingPlatformAdvance -> MovingPlatform
movingPlatformBeforeAdvance adv =
  let mp = mpaPlatform adv
      pos = movingPlatformPos mp
   in mp
        { movingPlatformPos =
            position (posX pos - mpaDeltaX adv) (posY pos - mpaDeltaY adv)
        }

nudgePlayer :: Float -> Float -> Player -> Player
nudgePlayer dx dy p =
  let pos = playerPos p
   in p{playerPos = position (posX pos + dx) (posY pos + dy)}
