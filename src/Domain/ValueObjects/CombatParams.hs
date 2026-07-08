module Domain.ValueObjects.CombatParams (
  CombatParams (..),
)
where

import GHC.Generics (Generic)

import Domain.ValueObjects.Damage (Damage)
import Domain.ValueObjects.Frames (Frames)

data CombatParams = CombatParams
  { cpAttackDuration :: Frames
  , cpInvincibilityDuration :: Frames
  , cpContactDamage :: Damage
  , cpMeleeReach :: Float
  -- ^ Alcance base del melee en px. La hitbox real puede pasarse de acá según el arco del golpe (ver Domain.Logic.MeleeSwing).
  , cpMeleeDamage :: Damage
  , cpEnemyHurtFlashDuration :: Frames
  }
  deriving (Eq, Show, Generic)
