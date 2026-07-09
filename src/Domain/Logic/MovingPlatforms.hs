module Domain.Logic.MovingPlatforms (
  MovingPlatformAdvance (..),
  advanceMovingPlatforms,
  applyPrePhysicsCarry,
  allCollisionPlatforms,
)
where

import Data.List (foldl')

import Domain.Logic.Collision (playerRidingPlatformTop)
import Domain.Model.MovingPlatform (
  MovingPlatform (..),
  movingPlatformAsPlatform,
  movingPlatformIsHorizontal,
 )
import Domain.Model.Platform (Platform)
import Domain.Model.Player (Player (..), playerOnGround, playerPos)
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Position (Position, posX, posY, position, translate)
import Domain.ValueObjects.Tolerance (epsilon)

data MovingPlatformAdvance = MovingPlatformAdvance
  { mpaPlatform :: MovingPlatform
  , mpaDeltaX :: Float
  , mpaDeltaY :: Float
  }
  deriving (Eq, Show)

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
        if movingPlatformIsHorizontal mp
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

moveAlongAxis :: Float -> Float -> Float -> (Float, Bool)
moveAlongAxis cur target dist
  | abs (target - cur) <= dist + epsilon = (target, True)
  | otherwise =
      let dir = signum (target - cur)
       in (cur + dir * dist, False)

allCollisionPlatforms :: [Platform] -> [MovingPlatform] -> [Platform]
allCollisionPlatforms static moving =
  static ++ map movingPlatformAsPlatform moving

-- Arrastra a los pasajeros: un jugador parado sobre una plataforma se desplaza el mismo delta antes
-- de correr la física, así la plataforma no se le escurre por debajo.
applyPrePhysicsCarry :: Player -> [MovingPlatformAdvance] -> Player
applyPrePhysicsCarry =
  foldl' applyOne
 where
  applyOne player adv =
    let oldMp = movingPlatformBeforeAdvance adv
     in if playerOnGround player
          && playerRidingPlatformTop player (movingPlatformAsPlatform oldMp)
          then nudgePlayer (mpaDeltaX adv) (mpaDeltaY adv) player
          else player

-- Rebobina a la posición previa al movimiento de la plataforma para que "¿el jugador iba encima?" se pruebe
-- contra donde estaba antes de moverse, no después.
movingPlatformBeforeAdvance :: MovingPlatformAdvance -> MovingPlatform
movingPlatformBeforeAdvance adv =
  let mp = mpaPlatform adv
   in mp
        { movingPlatformPos =
            translate (-(mpaDeltaX adv)) (-(mpaDeltaY adv)) (movingPlatformPos mp)
        }

nudgePlayer :: Float -> Float -> Player -> Player
nudgePlayer dx dy p =
  p{playerPos = translate dx dy (playerPos p)}
