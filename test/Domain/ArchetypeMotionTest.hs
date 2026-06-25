-- | 'motionForArchetype' y 'applyTuning' sobre stats (no sobre 'BehaviourProgram').
module Domain.ArchetypeMotionTest where

-- Grupo 2 — third-party
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

-- Grupo 3 — proyecto
import Domain.Logic.BehaviourCatalog (applyTuning, motionForArchetype)
import Domain.Model.EnemyKind (
  EnemyKind (ArcherKind, BatKind, GolemKind, SnailKind),
  EnemyKindStats (eksMotion),
  EnemyMotionStats (..),
  enemyKindStats,
 )
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype, GuardArchetype, PatrolArchetype),
 )
import Domain.ValueObjects.Amplifier (identityAmplifier, mkAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Frames (frames)
import Domain.ValueObjects.Multiplier (mkMultiplier)

unit_snailChaseBecomesGroundReactive :: Assertion
unit_snailChaseBecomesGroundReactive =
  motionForArchetype SnailKind ChaseArchetype
    @?= ReactiveMotion 30 (30 * 0.8) 140 28

unit_snailGuardIsTighterThanChase :: Assertion
unit_snailGuardIsTighterThanChase =
  motionForArchetype SnailKind GuardArchetype
    @?= ReactiveMotion 30 (30 * 0.8) 60 10

unit_chaseDiffersFromGuard :: Assertion
unit_chaseDiffersFromGuard =
  assertBool
    "chase y guard deben producir stats distintos"
    ( motionForArchetype SnailKind ChaseArchetype
        /= motionForArchetype SnailKind GuardArchetype
    )

unit_snailPatrolKeepsNaturalLeg :: Assertion
unit_snailPatrolKeepsNaturalLeg =
  motionForArchetype SnailKind PatrolArchetype
    @?= PatrolMotion 30 (frames 90)

unit_golemPatrolIsSynthesised :: Assertion
unit_golemPatrolIsSynthesised =
  motionForArchetype GolemKind PatrolArchetype
    @?= PatrolMotion 25 (frames 90)

unit_batChaseStaysAerial :: Assertion
unit_batChaseStaysAerial =
  motionForArchetype BatKind ChaseArchetype
    @?= FlyingReactiveMotion 80 (80 * 0.8) 140 28 80 (frames 60)

unit_archerKeepsItsMotion :: Assertion
unit_archerKeepsItsMotion =
  motionForArchetype ArcherKind ChaseArchetype
    @?= eksMotion (enemyKindStats ArcherKind)

unit_applyTuningScalesReactive :: Assertion
unit_applyTuningScalesReactive =
  applyTuning
    (BehaviourTuning (mkMultiplier 2.0) (mkAmplifier 2.0) identityAmplifier)
    (ReactiveMotion 30 24 140 28)
    @?= ReactiveMotion 60 48 280 56

unit_applyTuningPatrolScalesSpeedKeepsLeg :: Assertion
unit_applyTuningPatrolScalesSpeedKeepsLeg =
  applyTuning
    (BehaviourTuning (mkMultiplier 0.5) (mkAmplifier 3.0) identityAmplifier)
    (PatrolMotion 30 (frames 90))
    @?= PatrolMotion 15 (frames 90)

unit_applyTuningArcherScalesShootRange :: Assertion
unit_applyTuningArcherScalesShootRange =
  applyTuning
    (BehaviourTuning (mkMultiplier 2.0) (mkAmplifier 2.0) identityAmplifier)
    (ArcherMotion 160 (frames 90) 200 (frames 120) 8 8)
    @?= ArcherMotion 320 (frames 90) 200 (frames 120) 8 8
