module Domain.Logic.FallingHazards (
  resolveFallingHazards,
)
where

import Data.List (foldl')

import Domain.Logic.PlayerLife (applyContactDamage, deathLineY)
import Domain.Model.FallingHazard (
  FallingHazard (..),
  FallingHazardPhase (..),
  fallingHazardAabb,
  fallingHazardIsActive,
 )
import Domain.Model.Player (
  Player (..),
  playerAabb,
 )
import Domain.Model.World (World (..))
import Domain.ValueObjects.Aabb (aabbOverlaps)
import Domain.ValueObjects.CombatParams (CombatParams)
import Domain.ValueObjects.DeltaTime (DeltaTime, seconds)
import Domain.ValueObjects.Frames (hasFramesLeft, tickFrames)
import Domain.ValueObjects.LifeParams (LifeParams)
import Domain.ValueObjects.Position (positionBelowY, translate)

resolveFallingHazards ::
  LifeParams ->
  CombatParams ->
  DeltaTime ->
  World ->
  World
resolveFallingHazards lp cp dt w =
  let despawnLine = deathLineY lp w - hazardDespawnDrop
      hazards = worldFallingHazards w
      player' = foldl' (damageFromFalling cp dt) (worldPlayer w) hazards
      active =
        filter fallingHazardIsActive $
          map (advanceHazard dt despawnLine) hazards
   in w{worldFallingHazards = active, worldPlayer = player'}

-- Cuánto (px) por debajo de la línea de muerte sigue cayendo un hazard antes de despawnear.
hazardDespawnDrop :: Float
hazardDespawnDrop = 200

advanceHazard :: DeltaTime -> Float -> FallingHazard -> FallingHazard
advanceHazard dt despawnLine h =
  case fallingHazardPhase h of
    HazardFalling ->
      let moved = moveDown dt h
       in if positionBelowY despawnLine (fallingHazardPos moved)
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
  | not (fallingHazardWillHitPlayer dt h player) = player
  | otherwise = applyContactDamage cp player

-- Daño según la posición del hazard en el próximo paso, no la actual.
fallingHazardWillHitPlayer :: DeltaTime -> FallingHazard -> Player -> Bool
fallingHazardWillHitPlayer dt h player =
  aabbOverlaps (fallingHazardAabb (moveDown dt h)) (playerAabb player)
