{- | El build aplica el tuning resuelto: 'toughness×' escala la salud del enemigo
(un caracol base de 1 HP con toughness ×3 nace con 3).
-}
module Domain.BuildEnemyTuningTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Logic.BuildWorld (buildEnemy)
import Domain.Model.Enemy (enemyMaxHealth)
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  EnemyDef (..),
 )
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Multiplier (identityMultiplier, mkMultiplier)
import Domain.ValueObjects.Position (position)

unit_buildScalesHealthByToughness :: Assertion
unit_buildScalesHealthByToughness =
  fmap (healthPoints . enemyMaxHealth) (buildEnemy toughSnail) @?= Right 3
 where
  toughSnail =
    EnemyDef
      { enemyDefId = 1
      , enemyDefKind = SnailKind
      , enemyDefPos = position 0 8
      , enemyDefBehaviourPreset = Just ChaseArchetype
      , enemyDefBehaviourHint = Nothing
      , enemyDefBehaviourTuning =
          Just (BehaviourTuning identityMultiplier identityMultiplier (mkMultiplier 3.0))
      }
