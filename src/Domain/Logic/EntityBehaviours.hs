{- | Programas de comportamiento compuestos (catálogo DSL).

Construidos con los primitivos de @Domain.Model.EntityBehaviour@.
-}
module Domain.Logic.EntityBehaviours (
  patrolHorizontal,
  reactiveFsm,
  reactiveChase,
  reactiveGuard,
  defaultProgramForKind,
  programForArchetype,
)
where

import Data.Function (fix)

import Domain.Model.EnemyKind (EnemyKind (..), EnemyKindStats (..), enemyKindStats)
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
import Domain.ValueObjects.Velocity (velocity)

{- | Patrulla horizontal indefinidamente: velocidad @±speed@ durante @frames + 1@
  frames por tramo. Son @frames + 1@ y no @frames@ porque 'setVelocity' consume un
  behaviour step propio (fija la velocidad) y luego 'waitFrames' la mantiene @frames@
  frames más. Sobre suelo plano, cinemática M6. Requiere @speed > 0@ y @frames > 0@.
-}
patrolHorizontal :: Float -> Int -> BehaviourProgram
patrolHorizontal speed frames
  | speed > 0 && frames > 0 =
      fix $ \p ->
        setVelocity (velocity (-speed) 0)
          >>> waitFrames frames
          >>> setVelocity (velocity speed 0)
          >>> waitFrames frames
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
          (moveTowardPlayer chaseSpeed >>> waitFrames 1 >>> loop)
          ( ifNearSpawn
              spawnRadius
              (facePlayer >>> waitFrames 1 >>> loop)
              (moveTowardSpawn returnSpeed >>> waitFrames 1 >>> loop)
          )
  | otherwise = idleProgram

-- | Preset chase (Bat): alias de 'reactiveFsm' con stats del kind.
reactiveChase :: EnemyKindStats -> BehaviourProgram
reactiveChase stats =
  reactiveFsm
    (eksChaseRange stats)
    (eksChaseSpeed stats)
    (eksReturnSpeed stats)
    (eksSpawnRadius stats)

-- | Preset guard (Golem): misma máquina; idle en spawn es guardar + mirar.
reactiveGuard :: EnemyKindStats -> BehaviourProgram
reactiveGuard = reactiveChase

-- | Programa por defecto según clase de enemigo.
defaultProgramForKind :: EnemyKind -> BehaviourProgram
defaultProgramForKind kind =
  let stats = enemyKindStats kind
   in case kind of
        SnailKind ->
          patrolHorizontal (eksPatrolSpeed stats) (eksPatrolFrames stats)
        _ -> reactiveChase stats

-- | Programa según arquetipo explícito y stats de la clase.
programForArchetype :: EnemyKind -> BehaviourArchetype -> BehaviourProgram
programForArchetype kind archetype =
  let stats = enemyKindStats kind
   in case archetype of
        PatrolArchetype ->
          patrolHorizontal (eksPatrolSpeed stats) (eksPatrolFrames stats)
        ChaseArchetype -> reactiveChase stats
        GuardArchetype -> reactiveGuard stats
