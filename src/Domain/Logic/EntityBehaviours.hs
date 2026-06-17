{- | Programas de comportamiento compuestos (catálogo DSL).

Construidos con los primitivos de @Domain.Model.EntityBehaviour@.
-}
module Domain.Logic.EntityBehaviours (
  patrolHorizontal,
  reactiveFsm,
  defaultProgramForKind,
  programForArchetype,
)
where

import Data.Function (fix)

import Domain.Model.EnemyKind (
  EnemyKind,
  EnemyKindStats (eksMotion),
  EnemyMotionStats (..),
  enemyKindStats,
 )
import Domain.Model.EntityBehaviour (
  BehaviourProgram,
  facePlayer,
  idleProgram,
  ifNearSpawn,
  ifPlayerWithinRange,
  moveTowardPlayer,
  moveTowardSpawn,
  setVelocity,
  waitFrames,
  (>>>),
 )
import Domain.Model.LevelDefinition (BehaviourArchetype (..))
import Domain.ValueObjects.Frames (Frames, frames, hasFramesLeft)
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

{- | Programa de comportamiento para una variante de movimiento.

El arquetipo chase y el arquetipo guard comparten la misma máquina reactiva
('reactiveFsm'), que en su rama de spawn ya mira al jugador; hoy no se distinguen
en comportamiento (la diferencia real vive en los stats por clase).
-}
programForMotion :: EnemyMotionStats -> BehaviourProgram
programForMotion (PatrolMotion speed legFrames) = patrolHorizontal speed legFrames
programForMotion (ReactiveMotion chaseSpeed returnSpeed chaseRange spawnRadius) =
  reactiveFsm chaseRange chaseSpeed returnSpeed spawnRadius

-- | Programa por defecto según clase de enemigo (su arquetipo natural de movimiento).
defaultProgramForKind :: EnemyKind -> BehaviourProgram
defaultProgramForKind = programForMotion . eksMotion . enemyKindStats

{- | Programa según arquetipo explícito y clase.

Si el arquetipo autorado no corresponde a la variante de movimiento de la clase
(p. ej. patrulla sobre una clase reactiva), degrada a 'idleProgram' — el mismo
resultado que antes producían los stats inaplicables en cero a través de las guardas.
-}
programForArchetype :: EnemyKind -> BehaviourArchetype -> BehaviourProgram
programForArchetype kind archetype =
  case (archetype, eksMotion (enemyKindStats kind)) of
    (PatrolArchetype, motion@PatrolMotion{}) -> programForMotion motion
    (ChaseArchetype, motion@ReactiveMotion{}) -> programForMotion motion
    (GuardArchetype, motion@ReactiveMotion{}) -> programForMotion motion
    _ -> idleProgram
