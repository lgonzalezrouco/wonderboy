-- | Avance, daño y ciclo de peligros ambientales que caen (puro).
module Domain.Logic.FallingHazards (
  resolveFallingHazards,
)
where

import Domain.Logic.PlayerLife (applyDamage, deathLineY)
import Domain.Model.FallingHazard (
  FallingHazard (..),
  FallingHazardPhase (..),
  fallingHazardAabb,
  fallingHazardIsActive,
 )
import Domain.Model.Player (
  Player (..),
  playerAabb,
  playerInvincibilityFrames,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (aabbOverlaps)
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.LifeParams (LifeParams)
import Domain.ValueObjects.Position (posY, translate)

-- | Avanza peligros, aplica daño por contacto y elimina ciclos terminados.
resolveFallingHazards ::
  LifeParams ->
  CombatParams ->
  DeltaTime ->
  World ->
  World
resolveFallingHazards lp cp dt w =
  let despawnLine = deathLineY lp w
      hazards = worldFallingHazards w
      player' = foldl (damageFromFalling cp dt) (worldPlayer w) hazards
      active =
        filter fallingHazardIsActive $
          map (advanceHazard dt despawnLine) hazards
   in w{worldFallingHazards = active, worldPlayer = player'}

advanceHazard :: DeltaTime -> Float -> FallingHazard -> FallingHazard
advanceHazard dt despawnLine h =
  case fallingHazardPhase h of
    HazardFalling ->
      let moved = moveDown dt h
       in if posY (fallingHazardPos moved) < despawnLine
            then despawnHazard h
            else moved
    HazardWaiting remaining ->
      if hasFramesLeft remaining
        then h{fallingHazardPhase = HazardWaiting (tickFrames remaining)}
        else
          h
            { fallingHazardPos = fallingHazardSpawnPos h
            , fallingHazardPhase = HazardFalling
            }
    HazardDone -> h

moveDown :: DeltaTime -> FallingHazard -> FallingHazard
moveDown dt h =
  let dy = fallingHazardFallSpeed h * seconds dt
   in h{fallingHazardPos = translate 0 (-dy) (fallingHazardPos h)}

despawnHazard :: FallingHazard -> FallingHazard
despawnHazard h =
  case fallingHazardLoopDelay h of
    Just delay ->
      h
        { fallingHazardPos = fallingHazardSpawnPos h
        , fallingHazardPhase = HazardWaiting delay
        }
    Nothing -> h{fallingHazardPhase = HazardDone}

damageFromFalling :: CombatParams -> DeltaTime -> Player -> FallingHazard -> Player
damageFromFalling cp dt player h
  | fallingHazardPhase h /= HazardFalling = player
  | not (aabbOverlaps (fallingHazardAabb (moveDown dt h)) (playerAabb player)) = player
  | hasFramesLeft (playerInvincibilityFrames player) = player
  | otherwise =
      applyDamage
        (cpContactDamage cp)
        player{playerInvincibilityFrames = cpInvincibilityDuration cp}
