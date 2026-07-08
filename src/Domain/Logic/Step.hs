module Domain.Logic.Step (
  advanceFrame,
  step,
)
where

import Domain.Logic.BossArena (advanceBossArenaEngagement, appendBossArenaWallsForPlayer, clampPlayerInBossArena)
import Domain.Logic.Collision (resolveEnemyPlatforms, resolvePlayerPlatforms)
import Domain.Logic.CrumblingPlatforms (
  advanceCrumblingPlatforms,
  appendEnemySolidCrumbling,
  appendPlayerSolidCrumbling,
 )
import Domain.Logic.MovingPlatforms (
  advanceMovingPlatforms,
  allCollisionPlatforms,
  applyPrePhysicsCarry,
  mpaPlatform,
 )
import Domain.Logic.Physics (
  applyEnemyGravity,
  applyGravity,
  applyHorizontalInput,
  applyJump,
  integrateEnemy,
  integratePlayer,
 )
import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.Model.Enemy (Enemy (..))
import Domain.Model.EnemyKind (isFlyingKind)
import Domain.Model.Platform (Platform, platformHeight)
import Domain.Model.Player (Player, playerOnGround, playerVel)
import Domain.Model.World (World (..))
import Domain.ValueObjects.DeltaTime (DeltaTime, deltaTime, isFrozen, seconds)
import Domain.ValueObjects.Input (Input)
import Domain.ValueObjects.LifeParams (LifeParams)
import Domain.ValueObjects.PhysicsParams (PhysicsParams)
import Domain.ValueObjects.Velocity (velY)

advanceFrame :: PhysicsParams -> LifeParams -> DeltaTime -> Input -> World -> World
advanceFrame params life dt input w
  | isFrozen dt = w
  | otherwise = step params life dt input (runBehaviourStep w)

step :: PhysicsParams -> LifeParams -> DeltaTime -> Input -> World -> World
step _ _ dt _ w | isFrozen dt = w
step params life dt input w =
  let p0 = worldPlayer w
      wCrumble = advanceCrumblingPlatforms life dt p0 w
      advances = advanceMovingPlatforms dt (worldMovingPlatforms wCrumble)
      moving = map mpaPlatform advances
      w' = wCrumble{worldMovingPlatforms = moving}
      p0carried = applyPrePhysicsCarry p0 advances
      wasOnGround = playerOnGround p0carried
      p1 = applyHorizontalInput params input p0carried
      p2 = applyGravity params dt p1
      p3 = applyJump params input wasOnGround p2
      vyAtCollide = velY (playerVel p3)
      crumbling = worldCrumblingPlatforms w'
      basePlats = allCollisionPlatforms (worldPlatforms w') moving
      playerPlats =
        appendBossArenaWallsForPlayer w' (appendPlayerSolidCrumbling basePlats crumbling)
      enemyPlats = appendEnemySolidCrumbling basePlats crumbling
      p4 = integrateAndCollide dt p3 playerPlats vyAtCollide
      p5 = clampPlayerInBossArena w' p4
      enemies' =
        map (integrateAndCollideEnemy params dt enemyPlats) (worldEnemies w')
      w1 = advanceBossArenaEngagement w'{worldPlayer = p5, worldEnemies = enemies'}
   in w1

integrateAndCollide :: DeltaTime -> Player -> [Platform] -> Float -> Player
integrateAndCollide dt p plats vyBefore =
  let n = substeps dt p plats
   in runSubsteps n (deltaTimeSub dt n) plats vyBefore p

integrateAndCollideEnemy ::
  PhysicsParams -> DeltaTime -> [Platform] -> Enemy -> Enemy
integrateAndCollideEnemy params dt plats e
  | isFlyingKind (enemyKind e) = integrateEnemy dt e
  | otherwise =
      let e1 = applyEnemyGravity params dt e
          vyBefore = velY (enemyVel e1)
          e2 = integrateEnemy dt e1
       in resolveEnemyPlatforms plats vyBefore e2

runSubsteps :: Int -> DeltaTime -> [Platform] -> Float -> Player -> Player
runSubsteps n dtSub plats vyBefore p
  | n <= 0 = p
  | otherwise =
      -- seq mantiene cada substep estricto para que la recursión no acumule thunks.
      let p' = resolvePlayerPlatforms plats vyBefore (integratePlayer dtSub p)
       in p' `seq` runSubsteps (n - 1) dtSub plats vyBefore p'

-- Tope duro de substeps para que una velocidad extrema no dispare el trabajo por frame.
maxSubsteps :: Int
maxSubsteps = 16

-- Piso positivo: si platformHeight <= 0, maxStep sería 0 y dy/0 daría NaN en el ceiling de abajo.
minSubstep :: Float
minSubstep = 1e-3

-- Parte el movimiento vertical del frame en substeps no mayores a la mitad de la
-- plataforma más fina, así una caída rápida no la atraviesa de una sola integración.
substeps :: DeltaTime -> Player -> [Platform] -> Int
substeps dt p plats =
  let t = seconds dt
      dy = abs (velY (playerVel p)) * t
      maxStep = max minSubstep (minPlatformHeight plats * 0.5)
      raw = ceiling (dy / maxStep) :: Int
   in clampSubsteps raw

deltaTimeSub :: DeltaTime -> Int -> DeltaTime
deltaTimeSub dt n =
  deltaTime (seconds dt / fromIntegral (max 1 n))

clampSubsteps :: Int -> Int
clampSubsteps n = max 1 (min maxSubsteps n)

-- Sin plataformas: una altura enorme colapsa el cálculo a un único substep grueso.
minPlatformHeight :: [Platform] -> Float
minPlatformHeight [] = 1e6
minPlatformHeight plats = minimum (platformHeight <$> plats)
