{- | Programas de comportamiento compuestos (catálogo DSL).

Construidos con los primitivos de @Domain.Model.EntityBehaviour@.
-}
module Domain.Logic.EntityBehaviours (
  patrolHorizontal,
  reactiveFsm,
  flyingReactiveFsm,
  airPatrolFacePlayer,
  archerProgram,
  defaultProgramForKind,
  programForArchetype,
  motionForArchetype,
  applyTuning,
  programForArchetypeTuned,
)
where

import Data.Function (fix)

import Domain.Model.EnemyKind (
  EnemyKind,
  EnemyKindStats (eksMotion),
  EnemyMotionStats (..),
  enemyKindStats,
  isFlyingKind,
 )
import Domain.Model.EntityBehaviour (
  BehaviourProgram,
  facePlayer,
  idleProgram,
  ifNearSpawn,
  ifPlayerWithinRange,
  moveToward,
  moveTowardPlayer,
  moveTowardSpawn,
  setFacingTowardPlayer,
  setVelocity,
  shoot,
  waitFrames,
  (>>>),
 )
import Domain.Model.LevelDefinition (BehaviourArchetype (..))
import Domain.ValueObjects.Amplifier (unAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Frames (Frames, frames, hasFramesLeft)
import Domain.ValueObjects.Multiplier (unMultiplier)
import Domain.ValueObjects.Velocity (velocity)

{- | Patrulla horizontal indefinidamente: velocidad @±speed@ durante @frames + 1@
  frames por tramo. Son @frames + 1@ y no @frames@ porque 'setVelocity' consume un
  behaviour step propio (fija la velocidad) y luego 'waitFrames' la mantiene @frames@
  frames más. Sobre suelo plano, cinemática M6. Requiere @speed > 0@ y @frames > 0@.
-}
patrolHorizontal :: Float -> Frames -> BehaviourProgram
patrolHorizontal speed legFrames
  | speed > 0 && hasFramesLeft legFrames =
      fix $ \p ->
        setVelocity (velocity (-speed) 0)
          >>> waitFrames legFrames
          >>> setVelocity (velocity speed 0)
          >>> waitFrames legFrames
          >>> p
  | otherwise = idleProgram

{- | FSM reactivo: chase en rango, idle en spawn, retorno fuera de rango.

Re-evalúa sensores cada behaviour step vía @fix@.
-}
reactiveFsm ::
  Float ->
  Float ->
  Float ->
  Float ->
  BehaviourProgram
reactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius
  | chaseRange > 0 && chaseSpeed > 0 && returnSpeed > 0 && spawnRadius > 0 =
      fix $ \loop ->
        ifPlayerWithinRange
          chaseRange
          (moveTowardPlayer chaseSpeed >>> waitFrames (frames 1) >>> loop)
          ( ifNearSpawn
              spawnRadius
              (facePlayer >>> waitFrames (frames 1) >>> loop)
              (moveTowardSpawn returnSpeed >>> waitFrames (frames 1) >>> loop)
          )
  | otherwise = idleProgram

{- | FSM reactivo aéreo: persigue y regresa en horizontal; en spawn patrulla en X.

Mantiene la altitud de spawn (sin planeo vertical). Los murciélagos ignoran
colisión con plataformas en @Domain.Logic.Collision@.
-}
flyingReactiveFsm ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Frames ->
  BehaviourProgram
flyingReactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius patrolSpeed patrolLeg
  | chaseRange > 0
      && chaseSpeed > 0
      && returnSpeed > 0
      && spawnRadius > 0
      && patrolSpeed > 0
      && hasFramesLeft patrolLeg =
      fix $ \loop ->
        ifPlayerWithinRange
          chaseRange
          (moveToward chaseSpeed >>> waitFrames (frames 1) >>> loop)
          ( ifNearSpawn
              spawnRadius
              (airPatrolFacePlayer patrolSpeed patrolLeg >>> loop)
              (moveTowardSpawn returnSpeed >>> waitFrames (frames 1) >>> loop)
          )
  | otherwise = idleProgram

-- | Patrulla horizontal en el aire mirando al jugador (sin desplazamiento vertical).
airPatrolFacePlayer :: Float -> Frames -> BehaviourProgram
airPatrolFacePlayer patrolSpeed patrolLeg
  | patrolSpeed > 0 && hasFramesLeft patrolLeg =
      setFacingTowardPlayer
        >>> setVelocity (velocity (-patrolSpeed) 0)
        >>> waitFrames patrolLeg
        >>> setFacingTowardPlayer
        >>> setVelocity (velocity patrolSpeed 0)
        >>> waitFrames patrolLeg
  | otherwise = facePlayer

-- | Archer: dispara en rango, mira al jugador y espera cooldown entre disparos.
archerProgram :: Float -> Frames -> BehaviourProgram
archerProgram shootRange cooldown
  | shootRange > 0 && hasFramesLeft cooldown =
      fix $ \loop ->
        ifPlayerWithinRange
          shootRange
          (facePlayer >>> shoot >>> waitFrames cooldown >>> loop)
          (facePlayer >>> waitFrames (frames 1) >>> loop)
  | otherwise = idleProgram

programForMotion :: EnemyMotionStats -> BehaviourProgram
programForMotion (PatrolMotion speed legFrames) = patrolHorizontal speed legFrames
programForMotion (ReactiveMotion chaseSpeed returnSpeed chaseRange spawnRadius) =
  reactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius
programForMotion (FlyingReactiveMotion chaseSpeed returnSpeed chaseRange spawnRadius patrolSpeed patrolLeg) =
  flyingReactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius patrolSpeed patrolLeg
programForMotion (ArcherMotion shootRange cooldown _ _ _ _) =
  archerProgram shootRange cooldown

-- | Programa por defecto según clase de enemigo (su arquetipo natural de movimiento).
defaultProgramForKind :: EnemyKind -> BehaviourProgram
defaultProgramForKind = programForMotion . eksMotion . enemyKindStats

{- | Cualquier clase puede expresar cualquier arquetipo: sintetiza stats y reutiliza
'programForMotion'.
-}
programForArchetype :: EnemyKind -> BehaviourArchetype -> BehaviourProgram
programForArchetype kind = programForMotion . motionForArchetype kind

{- | Materializa un arquetipo sobre una clase: velocidad de su motion natural, rangos
por arquetipo (chase amplio, guard corto). El arquero conserva 'ArcherMotion'.
-}
motionForArchetype :: EnemyKind -> BehaviourArchetype -> EnemyMotionStats
motionForArchetype kind archetype =
  case eksMotion (enemyKindStats kind) of
    motion@ArcherMotion{} -> motion
    naturalMotion ->
      case archetype of
        PatrolArchetype -> patrolFrom naturalMotion
        ChaseArchetype -> reactiveFrom naturalMotion chaseDetectRange chaseRoamRadius
        GuardArchetype -> reactiveFrom naturalMotion guardDetectRange guardHoldRadius
 where
  patrolFrom (PatrolMotion s legs) = PatrolMotion s legs
  patrolFrom motion = PatrolMotion (baseSpeed motion) defaultPatrolLeg

  reactiveFrom motion range hold =
    let speed = baseSpeed motion
        returnSpeed = speed * returnSpeedFactor
     in if isFlyingKind kind
          then FlyingReactiveMotion speed returnSpeed range hold speed homePatrolLeg
          else ReactiveMotion speed returnSpeed range hold

{- | speed× y reach× sobre velocidades y rangos; frames y proyectil intactos.
toughness× se aplica en 'BuildWorld'.
-}
applyTuning :: BehaviourTuning -> EnemyMotionStats -> EnemyMotionStats
applyTuning tuning motion = case motion of
  PatrolMotion s leg -> PatrolMotion (s * spd) leg
  ReactiveMotion cs rs cr sr ->
    ReactiveMotion (cs * spd) (rs * spd) (cr * rch) (sr * rch)
  FlyingReactiveMotion cs rs cr sr ps leg ->
    FlyingReactiveMotion (cs * spd) (rs * spd) (cr * rch) (sr * rch) (ps * spd) leg
  ArcherMotion shootRange cd projSpeed projLife w h ->
    ArcherMotion (shootRange * rch) cd projSpeed projLife w h
 where
  spd = unMultiplier (tuningSpeed tuning)
  rch = unAmplifier (tuningReach tuning)

programForArchetypeTuned ::
  EnemyKind -> BehaviourArchetype -> BehaviourTuning -> BehaviourProgram
programForArchetypeTuned kind archetype tuning =
  programForMotion (applyTuning tuning (motionForArchetype kind archetype))

-- | 'ArcherMotion' → 0 solo para totalidad bajo @-Wall@ (inalcanzable vía 'motionForArchetype').
baseSpeed :: EnemyMotionStats -> Float
baseSpeed (PatrolMotion s _) = s
baseSpeed (ReactiveMotion s _ _ _) = s
baseSpeed (FlyingReactiveMotion s _ _ _ _ _) = s
baseSpeed ArcherMotion{} = 0

returnSpeedFactor :: Float
returnSpeedFactor = 0.8

chaseDetectRange :: Float
chaseDetectRange = 140

chaseRoamRadius :: Float
chaseRoamRadius = 28

guardDetectRange :: Float
guardDetectRange = 60

guardHoldRadius :: Float
guardHoldRadius = 10

defaultPatrolLeg :: Frames
defaultPatrolLeg = frames 90

homePatrolLeg :: Frames
homePatrolLeg = frames 60
