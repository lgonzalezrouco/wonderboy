{- | Tests de 'motionForArchetype': cualquier clase de enemigo puede ahora expresar
cualquier arquetipo de comportamiento (patrol, chase, guard).

Se compara a nivel de 'EnemyMotionStats' —que deriva 'Eq'— en vez de a nivel de
'BehaviourProgram', que a propósito no tiene 'Eq' (es un 'Free' posiblemente cíclico,
ver @Domain.Model.EntityBehaviour@). Validar los stats sintetizados alcanza: la
maquinaria de 'programForMotion' que los consume ya está cubierta por otros tests.

Lo que se valida: la velocidad sale del repertorio natural de la clase, la locomoción
es terrestre o aérea según la clase, y los rangos distinguen chase de guard.
-}
module Domain.ArchetypeMotionTest where

-- Grupo 2 — third-party
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

-- Grupo 3 — proyecto
import Domain.Logic.EntityBehaviours (applyTuning, motionForArchetype)
import Domain.Model.EnemyKind (
  EnemyKind (ArcherKind, BatKind, GolemKind, SnailKind),
  EnemyKindStats (eksMotion),
  EnemyMotionStats (..),
  enemyKindStats,
 )
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype, GuardArchetype, PatrolArchetype),
 )
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Multiplier (identityMultiplier, mkMultiplier)

{- | Caracol (clase terrestre que solo patrullaba) con @chase@: antes quedaba quieto,
ahora sintetiza un FSM reactivo terrestre a su velocidad natural (30).
-}
unit_snailChaseBecomesGroundReactive :: Assertion
unit_snailChaseBecomesGroundReactive =
  motionForArchetype SnailKind ChaseArchetype
    @?= ReactiveMotion 30 (30 * 0.8) 140 28

{- | @guard@ usa rango de detección y radio de spawn más chicos que @chase@: el
enemigo custodia su puesto en vez de perseguir de lejos.
-}
unit_snailGuardIsTighterThanChase :: Assertion
unit_snailGuardIsTighterThanChase =
  motionForArchetype SnailKind GuardArchetype
    @?= ReactiveMotion 30 (30 * 0.8) 60 10

{- | @chase@ y @guard@ ya no son indistinguibles: difieren en los stats sintetizados
(antes compartían el mismo FSM sin diferencia observable).
-}
unit_chaseDiffersFromGuard :: Assertion
unit_chaseDiffersFromGuard =
  assertBool
    "chase y guard deben producir stats distintos"
    ( motionForArchetype SnailKind ChaseArchetype
        /= motionForArchetype SnailKind GuardArchetype
    )

{- | Patrulla sobre una clase que ya patrulla: conserva su tramo natural en lugar de
sintetizar uno nuevo.
-}
unit_snailPatrolKeepsNaturalLeg :: Assertion
unit_snailPatrolKeepsNaturalLeg =
  motionForArchetype SnailKind PatrolArchetype
    @?= PatrolMotion 30 (frames 90)

{- | Golem (clase reactiva, no patrulla por defecto) con @patrol@: sintetiza una
patrulla a su velocidad natural (25) con el tramo estándar.
-}
unit_golemPatrolIsSynthesised :: Assertion
unit_golemPatrolIsSynthesised =
  motionForArchetype GolemKind PatrolArchetype
    @?= PatrolMotion 25 (frames 90)

{- | Murciélago (clase voladora) con @chase@: el FSM reactivo sintetizado es __aéreo__
(persigue en 2D, mantiene altitud) a su velocidad natural (80).
-}
unit_batChaseStaysAerial :: Assertion
unit_batChaseStaysAerial =
  motionForArchetype BatKind ChaseArchetype
    @?= FlyingReactiveMotion 80 (80 * 0.8) 140 28 80 (frames 60)

{- | El arquero conserva su esencia (ataque a distancia): cualquier arquetipo de
movimiento lo deja con su 'ArcherMotion' natural.
-}
unit_archerKeepsItsMotion :: Assertion
unit_archerKeepsItsMotion =
  motionForArchetype ArcherKind ChaseArchetype
    @?= eksMotion (enemyKindStats ArcherKind)

{- | speed× y reach× escalan las velocidades y los rangos de ReactiveMotion
respectivamente; en este caso speed×2 y reach×0.5 sobre (30, 24, 140, 28).
-}
unit_applyTuningScalesReactive :: Assertion
unit_applyTuningScalesReactive =
  applyTuning
    (BehaviourTuning (mkMultiplier 2.0) (mkMultiplier 0.5) identityMultiplier)
    (ReactiveMotion 30 24 140 28)
    @?= ReactiveMotion 60 48 70 14

{- | speed×0.5 divide la velocidad de patrulla; los Frames del tramo no se tocan.
reach× no afecta a PatrolMotion porque no tiene rangos reactivos.
-}
unit_applyTuningPatrolScalesSpeedKeepsLeg :: Assertion
unit_applyTuningPatrolScalesSpeedKeepsLeg =
  applyTuning
    (BehaviourTuning (mkMultiplier 0.5) (mkMultiplier 3.0) identityMultiplier)
    (PatrolMotion 30 (frames 90))
    @?= PatrolMotion 15 (frames 90)

{- | En ArcherMotion solo reach× toca el shootRange; la velocidad del proyectil,
el cooldown y las dimensiones no se escalan.
-}
unit_applyTuningArcherScalesShootRange :: Assertion
unit_applyTuningArcherScalesShootRange =
  applyTuning
    (BehaviourTuning (mkMultiplier 2.0) (mkMultiplier 2.0) identityMultiplier)
    (ArcherMotion 160 (frames 90) 200 (frames 120) 8 8)
    @?= ArcherMotion 320 (frames 90) 200 (frames 120) 8 8
