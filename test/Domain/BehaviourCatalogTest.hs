module Domain.BehaviourCatalogTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Fixtures (enemyFrom, floorWorld)
import Domain.Logic.BehaviourCatalog (
  defaultProgramForKind,
  programForArchetypeTuned,
  programForEnemyDef,
 )
import Domain.Logic.RunBehaviour (runBehaviourStep)
import Domain.Model.Enemy (Enemy, enemyVel, spawnEnemy)
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.EntityBehaviour (BehaviourProgram)
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  EnemyDef (..),
 )
import Domain.Model.Player (spawnPlayer)
import Domain.Model.World (World (..), defaultMaxHealth)
import Domain.ValueObjects.BehaviourTuning (identityTuning)
import Domain.ValueObjects.Position (position)
import Domain.ValueObjects.Velocity (velX)

unit_programForEnemyDefWithoutPresetMatchesDefault :: Assertion
unit_programForEnemyDefWithoutPresetMatchesDefault =
  velX (enemyVel eFromDef) @?= velX (enemyVel eFromDefault)
 where
  def = baseSnailDef
  eFromDef = behaviourStep (programForEnemyDef def)
  eFromDefault = behaviourStep (defaultProgramForKind SnailKind)

unit_programForEnemyDefWithPresetMatchesTuned :: Assertion
unit_programForEnemyDefWithPresetMatchesTuned =
  velX (enemyVel eFromDef) @?= velX (enemyVel eFromTuned)
 where
  def = baseSnailDef{enemyDefBehaviourPreset = Just ChaseArchetype}
  eFromDef = behaviourStep (programForEnemyDef def)
  eFromTuned =
    behaviourStep (programForArchetypeTuned SnailKind ChaseArchetype identityTuning)

behaviourStep :: BehaviourProgram -> Enemy
behaviourStep prog =
  enemyFrom . runBehaviourStep $
    worldWithPlayerNearSnail{worldEnemies = [spawnEnemy 1 SnailKind (position 50 0) prog]}

worldWithPlayerNearSnail :: World
worldWithPlayerNearSnail =
  floorWorld{worldPlayer = spawnPlayer defaultMaxHealth (position 100 0)}

baseSnailDef :: EnemyDef
baseSnailDef =
  EnemyDef
    { enemyDefId = 1
    , enemyDefKind = SnailKind
    , enemyDefPos = position 50 0
    , enemyDefBehaviourPreset = Nothing
    , enemyDefBehaviourHint = Nothing
    , enemyDefBehaviourTuning = Nothing
    }
