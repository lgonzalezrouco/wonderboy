module Domain.Logic.MeleeSwing (
  meleeImpactPhase,
  attackPhase,
  attackSwingAngle,
  meleeImpactFrameCount,
  isMeleeImpactFrame,
  meleeHitboxWhenImpact,
  meleeHitboxAtImpact,
  attackBodyLunge,
  attackCueHandInset,
  attackCueHeight,
)
where

import Domain.Model.Player (Player (..), playerAabb, playerAttackFrames, playerFacing)
import Domain.ValueObjects.Aabb (Aabb (..))
import Domain.ValueObjects.CombatParams (CombatParams (..))
import Domain.ValueObjects.Facing (Facing (..))
import Domain.ValueObjects.Frames (frameCount, hasFramesLeft)

-- Punto del swing (0–1) donde la hoja realmente conecta.
meleeImpactPhase :: Float
meleeImpactPhase = 0.55

-- Píxeles que el cuerpo se lanza hacia adelante en el frame de impacto.
attackBodyLunge :: Float
attackBodyLunge = 4

-- Alcance melee base en px. El hitbox real se extiende más allá de esto a lo largo del arco del swing.
attackCueHandInset :: Float
attackCueHandInset = 3

-- Largo de la hoja en px, compartido por la señal visual y el cálculo del alcance.
attackCueHeight :: Float
attackCueHeight = 42

-- Ángulos clave del arco del swing en grados: anticipación, impacto y remate.
attackStartDegrees :: Float
attackStartDegrees = -135

attackImpactDegrees :: Float
attackImpactDegrees = -92

attackFollowThroughDegrees :: Float
attackFollowThroughDegrees = -65

attackPhase :: CombatParams -> Player -> Maybe Float
attackPhase combatParams p
  | not (hasFramesLeft frames) = Nothing
  | otherwise = Just (clamp01 (fromIntegral elapsed / fromIntegral phaseSpan))
 where
  frames = playerAttackFrames p
  total = max 1 (frameCount (cpAttackDuration combatParams))
  elapsed = total - frameCount frames
  phaseSpan = max 1 (total - 1)

attackSwingAngle :: Float -> Float
attackSwingAngle phase
  | phase <= meleeImpactPhase =
      lerp attackStartDegrees attackImpactDegrees (smoothStep windupT)
  | otherwise =
      lerp attackImpactDegrees attackFollowThroughDegrees (easeInCubic recoveryT)
 where
  windupT = clamp01 (phase / meleeImpactPhase)
  recoveryT = clamp01 ((phase - meleeImpactPhase) / (1 - meleeImpactPhase))

meleeImpactFrameCount :: CombatParams -> Int
meleeImpactFrameCount cp =
  let total = frameCount (cpAttackDuration cp)
      phaseSpan = max 1 (total - 1)
      elapsedAtImpact = round (meleeImpactPhase * fromIntegral phaseSpan)
   in max 1 (total - elapsedAtImpact)

-- True en el único frame de impacto de un swing, así el daño melee pega una vez y no en cada frame.
isMeleeImpactFrame :: CombatParams -> Player -> Bool
isMeleeImpactFrame cp p =
  hasFramesLeft (playerAttackFrames p)
    && frameCount (playerAttackFrames p) == meleeImpactFrameCount cp

meleeHitboxWhenImpact :: CombatParams -> Player -> Maybe Aabb
meleeHitboxWhenImpact cp p
  | isMeleeImpactFrame cp p =
      Just (meleeHitboxAtImpact cp (playerAabb p) (playerFacing p))
  | otherwise =
      Nothing

meleeHitboxAtImpact :: CombatParams -> Aabb -> Facing -> Aabb
meleeHitboxAtImpact _cp body facing =
  let phase = meleeImpactPhase
      envelope = sin (pi * phase)
      faceScale = facingScale facing
      dx = faceScale * attackBodyLunge * envelope
      lunged = shiftAabbX dx body
      angle = attackSwingAngle phase
      bladeH = attackCueHeight * (1 + 0.08 * sin (pi * phase))
      angleRad = angle * pi / 180
      bladeReach = abs (bladeH * sin angleRad)
      reach = attackCueHandInset + bladeReach
   in extendMeleeReach reach lunged facing

shiftAabbX :: Float -> Aabb -> Aabb
shiftAabbX dx box =
  box
    { aabbMinX = aabbMinX box + dx
    , aabbMaxX = aabbMaxX box + dx
    }

extendMeleeReach :: Float -> Aabb -> Facing -> Aabb
extendMeleeReach reach box facing =
  case facing of
    FacingRight -> box{aabbMaxX = aabbMaxX box + reach}
    FacingLeft -> box{aabbMinX = aabbMinX box - reach}

facingScale :: Facing -> Float
facingScale facing =
  case facing of
    FacingLeft -> -1
    FacingRight -> 1

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

lerp :: Float -> Float -> Float -> Float
lerp from to t = from + (to - from) * t

smoothStep :: Float -> Float
smoothStep t = t * t * (3 - 2 * t)

easeInCubic :: Float -> Float
easeInCubic t = t ^ (3 :: Int)
