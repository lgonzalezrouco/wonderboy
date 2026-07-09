module Domain.Logic.CrumblingPlatforms (
  advanceCrumblingPlatforms,
  appendEnemySolidCrumbling,
  appendPlayerSolidCrumbling,
)
where

import Data.Maybe (mapMaybe)

import Domain.Logic.Collision (playerRidingPlatformTop)
import Domain.Logic.PlayerLife (deathLineY)
import Domain.Model.CrumblingPlatform (
  CrumblingPlatform (..),
  CrumblingPlatformPhase (..),
  crumbleCountdownFrames,
  crumbleFallSpeed,
  crumblingPlatformAsPlatform,
  crumblingPlatformSolidForPlayer,
 )
import Domain.Model.Platform (Platform)
import Domain.Model.Player (Player (..))
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.LifeParams (LifeParams)
import Domain.ValueObjects.Position (positionBelowY, translate)

advanceCrumblingPlatforms ::
  LifeParams ->
  DeltaTime ->
  Player ->
  World ->
  World
advanceCrumblingPlatforms lp dt player w =
  let despawnLine = deathLineY lp w
      active =
        mapMaybe
          (advanceOne dt despawnLine . tryTrigger player)
          (worldCrumblingPlatforms w)
   in w{worldCrumblingPlatforms = active}

tryTrigger :: Player -> CrumblingPlatform -> CrumblingPlatform
tryTrigger player cp
  | crumblingTriggeredByPlayer player cp =
      cp{crumblingPlatformPhase = CrumbleCountingDown crumbleCountdownFrames}
  | otherwise = cp

-- Jugador apoyado sobre una ledge intacta: arranca el countdown.
crumblingTriggeredByPlayer :: Player -> CrumblingPlatform -> Bool
crumblingTriggeredByPlayer player cp =
  CrumbleIntact == crumblingPlatformPhase cp
    && playerOnGround player
    && playerRidingPlatformTop player (crumblingPlatformAsPlatform cp)

advanceOne :: DeltaTime -> Float -> CrumblingPlatform -> Maybe CrumblingPlatform
advanceOne dt despawnLine cp =
  case crumblingPlatformPhase cp of
    CrumbleIntact -> Just cp
    CrumbleCountingDown remaining ->
      let next = tickFrames remaining
       in Just
            cp
              { crumblingPlatformPhase =
                  if hasFramesLeft next
                    then CrumbleCountingDown next
                    else CrumbleFalling
              }
    CrumbleFalling ->
      let moved = moveDown dt cp
       in if positionBelowY despawnLine (crumblingPlatformPos moved)
            then Nothing
            else Just moved

moveDown :: DeltaTime -> CrumblingPlatform -> CrumblingPlatform
moveDown dt cp =
  let dy = crumbleFallSpeed * seconds dt
   in cp{crumblingPlatformPos = translate 0 (-dy) (crumblingPlatformPos cp)}

-- El jugador atraviesa las plataformas una vez que empiezan a caer. Los enemigos siguen
-- colisionando con todas (ver appendEnemySolidCrumbling).
appendPlayerSolidCrumbling :: [Platform] -> [CrumblingPlatform] -> [Platform]
appendPlayerSolidCrumbling plats crumbling =
  plats ++ map crumblingPlatformAsPlatform (filter crumblingPlatformSolidForPlayer crumbling)

appendEnemySolidCrumbling :: [Platform] -> [CrumblingPlatform] -> [Platform]
appendEnemySolidCrumbling plats crumbling =
  plats ++ map crumblingPlatformAsPlatform crumbling
