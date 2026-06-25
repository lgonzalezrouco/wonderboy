-- | 'buildEnemy' aplica toughness× a la salud.
module Domain.BuildEnemyTuningTest where

import Test.Tasty.HUnit (Assertion, (@?=))

import Domain.Logic.BuildWorld (buildEnemy)
import Domain.Model.Enemy (enemyHealth, enemyMaxHealth)
import Domain.Model.EnemyKind (EnemyKind (SnailKind))
import Domain.Model.LevelDefinition (
  BehaviourArchetype (ChaseArchetype),
  EnemyDef (..),
 )
import Domain.ValueObjects.Amplifier (identityAmplifier, mkAmplifier)
import Domain.ValueObjects.BehaviourTuning (BehaviourTuning (..))
import Domain.ValueObjects.Health (healthPoints)
import Domain.ValueObjects.Multiplier (identityMultiplier)
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
          Just (BehaviourTuning identityMultiplier identityAmplifier (mkAmplifier 3.0))
      }

unit_buildScalesCurrentHealthByToughness :: Assertion
unit_buildScalesCurrentHealthByToughness =
  fmap (healthPoints . enemyHealth) (buildEnemy toughSnail) @?= Right 3
 where
  toughSnail =
    EnemyDef
      { enemyDefId = 1
      , enemyDefKind = SnailKind
      , enemyDefPos = position 0 8
      , enemyDefBehaviourPreset = Just ChaseArchetype
      , enemyDefBehaviourHint = Nothing
      , enemyDefBehaviourTuning =
          Just (BehaviourTuning identityMultiplier identityAmplifier (mkAmplifier 3.0))
      }

unit_noTuningKeepsBaseHealth :: Assertion
unit_noTuningKeepsBaseHealth =
  fmap (healthPoints . enemyHealth) (buildEnemy plainSnail) @?= Right 1
 where
  plainSnail =
    EnemyDef
      { enemyDefId = 1
      , enemyDefKind = SnailKind
      , enemyDefPos = position 0 8
      , enemyDefBehaviourPreset = Nothing
      , enemyDefBehaviourHint = Nothing
      , enemyDefBehaviourTuning = Nothing
      }
